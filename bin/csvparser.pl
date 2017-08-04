#!/usr/bin/env perl
use strict;

use Time::localtime;
use File::Basename;
use Cwd;
use Cwd 'abs_path';
use FindBin;

my $debugFile = "csvparser.debug";
my $opt_debug=0;
my $opt_param=0;
my $opt_autover=0;
my $arg_string;
my $arg;
my $csv_file="x.csv";
my @fields;
my $wr_file="x.csv.tmp";
my $temp;


##+++++++++++++++++++++++++++++++++++++++++++++
## Variables used globall for verilog
##+++++++++++++++++++++++++++++++++++++++++++++
my @ver_module;
push @ver_module,"module spi_registers (\n /*AUTOARG*/ \n";
push @ver_module,");\n";
push @ver_module,'`include "reg_parameters.v"';
push @ver_module,"\n";

##+++++++++++++++++++++++++++++++++++++++++++++
## Basic ports sections
##+++++++++++++++++++++++++++++++++++++++++++++
my @ver_ports;
$temp = "
input         sclk;
input         reset_n;
input  [15:0] address;
input         write_en;
input  [15:0] data_in;
output [15:0] data_out; 
reg    [15:0] data_out; 

// Input and output ports as part of the register interface 
";
push @ver_ports,$temp;

my @ver_out_ports_assign;
$temp ='
// Routing bit fields to output ports
';
push @ver_out_ports_assign,$temp;

##+++++++++++++++++++++++++++++++++++++++++++++
## Template sections
##+++++++++++++++++++++++++++++++++++++++++++++
my @template_reg;
$temp = "

/*
basic_reg AUTO_TEMPLATE (
.strobe (2'b11),
";
push @template_reg,$temp;

##+++++++++++++++++++++++++++++++++++++++++++++
## Always block to read data 
##+++++++++++++++++++++++++++++++++++++++++++++
my @ver_read_data;
$temp="
always @(*) begin
case (address) 
";
push @ver_read_data,$temp;

##+++++++++++++++++++++++++++++++++++++++++++++
## End of module declaration
##+++++++++++++++++++++++++++++++++++++++++++++
my @ver_end;
$temp='};
  // Any undefined registers return 0
  default: data_out = 16'."'d0;".'
endcase
end

endmodule
// Local Variables:
// verilog-library-directories:("." "./comp")
// End:
';
push @ver_end,$temp;


## If no argument passed then stop
unless (scalar(@ARGV)) { print "\nOops! Need a csv file!\n\n"; exit; }

# print out the arguments that were passed in as is
foreach my $arg (@ARGV) {
   # if there is a quoted string passed in make sure to put quotes around it
   if ($arg =~ /\S\s+\S/) { $arg_string .= sprintf ("\"%s\" ",$arg); }
   else                   { $arg_string .= sprintf ("%s ",$arg);     }
}


# START PARSING COMMAND LINE ARGUMENTS HERE
my $i = 0;

## Decode the csv file
for ($i = 0; $i <= $#ARGV; $i++) {
   if    ($ARGV[$i] =~ /^-(hel|H)/)           { help(); exit} 
   elsif ($ARGV[$i] =~ /^-dbg/)               { $opt_debug = 1;} 
   elsif ($ARGV[$i] =~ /^-param/)             { $opt_param = 1;} 
   elsif ($ARGV[$i] =~ /^-autover/)           { $opt_autover = 1;} 
   elsif ($ARGV[$i] eq "-f")                  { $csv_file = $ARGV[++$i]; }
   elsif ($ARGV[$i] eq "-o")                  { $wr_file = $ARGV[++$i]; }
   else  {die "Cannot decode option $ARGV[$i]"; }
}

# initialize special files
if ($opt_debug) { open (DBG,">$debugFile") or die "Can't append to $debugFile\n\n"; }


my $first=1;
my $prv_reg_addr= "";
my $prv_reg_name= "";

## Open the csv file 
open (my $TFILE, '<', $csv_file) or die "Cannot open file $csv_file in read mode";
my $lno = 0;
my @row;

my ($reg_addr,$reg_msb,$reg_lsb);
my ($reg_def,$reg_rwatr,$reg_reset);
my ($reg_field,$reg_danger,$reg_src);
my ($reg_cs,$reg_apin,$reg_name);;
my ($reg_offset,$bits,$bit_sz,$bit_dec);
my (@tp_arr,$tp);
my (@param_addr,@param_def,@ver_reg_inst);
my (@reg_inst,@reg_full_def,@reg_rd_field_data);
my $sticky_flag = 0;
my $final_flg = 0;
my $reg_name_det = 0;

## while (my $csv_line = $csv_parser->getline($FILE) ) {
while (my $csv_line = <$TFILE>) {
  $lno++; ## chomp $csv_line;

  ## if($lno >= 104) {
   ## $lno = $lno;  ## Keep it for debug
  ## }

  ## my @row = split ",",$line;
  ## my @row = @$csv_line; 
  undef @row;
  ## my @row = parse_line(q{,},0,$csv_line); 
  my @row = split ",",$csv_line;

  $reg_name_det = 0;
  $reg_addr   = $row[0]; $reg_msb    = $row[1]; $reg_lsb    = $row[2]; $reg_def    = hex($row[3]); 
  $reg_rwatr  = $row[4]; $reg_reset  = $row[5]; $reg_field  = $row[6]; $reg_danger = $row[7];
  $reg_src    = $row[8]; $reg_cs     = $row[9]; $reg_apin   = $row[10];

  ## Jump  to next if there is nothing on reg_addr
  next unless($reg_addr =~ m/\S/);

  ## Triage the script 
  $bits = "[$reg_msb:$reg_lsb]";
  if($reg_msb eq $reg_lsb) {
    $bits = "[$reg_msb]";
  }
  $bit_sz = $reg_msb-$reg_lsb+1;
  if($bit_sz > 1) {
    $bit_dec = sprintf("\[%2d:%2d\]",$bit_sz,0);
  } else {
    $bit_dec = ""; 
  }

  ## If there are exception lines do not process 
  if ($row[0] =~ /Offset/) {
    if($opt_debug) {print DBG "$lno : Found exception \n";} next;
  }
  
  ## Flag to check if the register name is hit
  if (!($reg_addr eq $prv_reg_addr)) {
    $prv_reg_addr = $reg_addr;  $prv_reg_name = $reg_name;
    $reg_name     = $reg_field; $reg_offset   = $reg_addr;


    ## Parameters for the address 
    $reg_addr =~ s/h//g;
    $tp = sprintf("parameter ADDR_%-40s = 16'h%04x;\n",$reg_name,$reg_addr);
    push (@param_addr,$tp);

    ## Parameters for the default value 
    if($sticky_flag) {
      add_param();
      add_inst();
    }
    add_data_out();

    ## Clear up for the next routine
		undef @reg_inst; my @reg_inst;
	  undef @reg_full_def; my @reg_full_def;
    undef @reg_rd_field_data; my @reg_rd_fied_data;

    if($opt_debug) {print DBG "$lno : Found new register $reg_name \n";}
    ## Print register offset 
    if($opt_debug) {print DBG "$lno : register name $reg_name with offset value register value is $reg_offset\n";}

		$sticky_flag = 1; $reg_name_det = 1; 
    next; ## Skip to next line
  } 

  ## assign statement for output 
	push(@reg_full_def,sprintf("%d'h%x",$bit_sz,$reg_def)) unless ($reg_name_det);
  $tp = ""; 
  $tp = sprintf("assign %-40s = dout_%s%s;\n",$reg_field,$reg_name,$bits) unless ($reg_field =~ /reser/i || $reg_rwatr =~ /RO/);
  push @ver_out_ports_assign,"$tp";

  ##   
  if($reg_field =~ /reser/i) {
    push @reg_rd_field_data,sprintf("%d'h0,",$bit_sz);
  } else {
    push @reg_rd_field_data,sprintf("%s%s,",$reg_field,$bit_dec); 
  };

  if($reg_rwatr =~ /RO/) {
    $tp = sprintf("input  %-15s %-40s;\n",$bit_dec,$reg_field);
  } else {
    $tp = sprintf("output %-15s %-40s;\n",$bit_dec,$reg_field);
  }
  push @ver_ports,$tp;

  ## Populate the 
  

 ## Triage if it is reserved field
 if($reg_field =~ /reserved/ || $reg_field =~ /Reser/) {
 }

 ## Search and replace string
 $reg_reset =~ s/0b//g;
 $reg_reset =~ s/0x//g;
 $reg_reset =~ s/[X]+/x/g;

}

## End stuff to populate
## copy the same name 
$prv_reg_name = $reg_name;
$final_flg = 1;
add_param(); add_inst();
add_data_out();
push @ver_ports,"/*AUTOWIRE*/";


close($TFILE);


## Finally print all the necessary
if($opt_param) {
  print @param_addr;
  print @param_def;
}

if($opt_autover) {
	print @ver_module;
	print @ver_ports;
  print @ver_reg_inst;
  print @ver_out_ports_assign;
  print @ver_read_data;
  print @ver_end;
}


## End of the main program
####################################################### 

##++++++++++++++++++++++++++++++++++++++++++++
## All subroutines for the module 
##++++++++++++++++++++++++++++++++++++++++++++

## Subroutine to add parameter 
sub add_param {
  ## parameters 
  $tp = sprintf("parameter DFLT_%-40s = {",$prv_reg_name);
	$tp .= join(",",reverse @reg_full_def);
	$tp .= "};\n";
	push (@param_def,$tp); 
}

## Subroutine to add parameter 
sub add_inst {
  push @reg_inst,@template_reg;
  push (@reg_inst,".reg_addr    (ADDR_$prv_reg_name\[\]),\n");
  push (@reg_inst,".data_out    (dout_$prv_reg_name\[\]),\n");
  push (@reg_inst,".rst_data_in (DFLT_$prv_reg_name\[\]),\n");
  push (@reg_inst,".\\(.*\\)    (\\1\[\]),\n");
  push (@ver_reg_inst,@reg_inst);
  add_inst_end();
}

sub add_inst_end {
  push @ver_reg_inst,"); */\n";
  push @ver_reg_inst,"basic_reg inst_$prv_reg_name ( /*AUTOINST*/ \n";
  push @ver_reg_inst,");\n\n\n";

}

sub add_data_out {
    if($sticky_flag) {
      undef @tp_arr; my @tp_arr;
      push @tp_arr,reverse @reg_rd_field_data;
      $tp = "@tp_arr"; $tp =~ s/,$//g;
      push @ver_read_data,$tp;
      push @ver_read_data,sprintf("};\n  ADDR_%-40s : data_out = {",$reg_name) unless ($final_flg);
    } else {
      push @ver_read_data,sprintf("  ADDR_%-40s : data_out = {",$reg_name) unless($final_flg);
    }
}

## Sub routine for help
sub help {
  print "csvparser [-dbg] [-param] [-autover] -f <csv file name> -o <output file> \n";
}

