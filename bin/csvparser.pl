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


## Variables used globall for verilog
my @ver_module;
push @ver_module,"module spi_registers (/*AUTOARG*/";
push @ver_module,");";

## Basic ports sections
push @ports,"input sclk";
push @ports,"input sclk";

my @ver_read_data;
push @ver_read_data,"always @(*) begin";
push @

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
my ($reg_def,$reg_attr,$reg_reset);
my ($reg_field,$reg_danger,$reg_src);
my ($reg_cs,$reg_apin,$reg_name);;
my ($reg_offset,$bits,$bit_sz,$tp);
my (@param_addr,@param_def,@ports);
my @reg_full_def;
my $sticky_flag = 0;
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

  $reg_addr   = $row[0]; 
  $reg_msb    = $row[1];
  $reg_lsb    = $row[2];
  $reg_def    = hex($row[3]); 
  $reg_attr   = $row[4];
  $reg_reset  = $row[5];
  $reg_field  = $row[6];
  $reg_danger = $row[7];
  $reg_src    = $row[8];
  $reg_cs     = $row[9];
  $reg_apin   = $row[10];

  ## Triage the script 
  $bits = "[$reg_msb:$reg_lsb]";
  if($reg_msb eq $reg_lsb) {
    $bits = "[$reg_msb]";
  }
  $bit_sz = $reg_msb-$reg_lsb+1;

  ## If there are exception lines do not process 
  if ($row[0] =~ /Offset/) {
    if($opt_debug) {print DBG "$lno : Found exception \n";} next;
  }
  
  ## Flag to check if the register name is hit
  if (!($reg_addr eq $prv_reg_addr)) {
    $prv_reg_addr = $reg_addr; 
		$prv_reg_name = $reg_name;
    $reg_name     = $reg_field;
    $reg_offset   = $reg_addr;

    ## Parameters for the address 
    $reg_addr =~ s/h//g;
    $tp = sprintf("parameter ADDR_%-40s = 16'h%04x;\n",$reg_name,$reg_addr);
    push (@param_addr,$tp);

    ## Parameters for the default value 
    if($sticky_flag) {
      $tp = sprintf("parameter DFLT_%-40s = {",$prv_reg_name);
			$tp .= join(",",reverse @reg_full_def);
			$tp .= "};\n";
		  push (@param_def,$tp); 
    }


    ## Clear up for the next routine
		undef @reg_full_def;
		my @reg_full_def;


    if($opt_debug) {print DBG "$lno : Found new register $reg_name \n";}

    ## Print register offset 
    if($opt_debug) {print DBG "$lno : register name $reg_name with offset value register value is $reg_offset\n";}

		$sticky_flag = 1;
		$reg_name_det = 1;
  } 

	push(@reg_full_def,sprintf("%d'h%x",$bit_sz,$reg_def)) unless ($reg_name_det);
  

 ## Triage if it is reserved field
 if($reg_field =~ /reserved/ || $reg_field =~ /Reser/) {
 }

 ## Search and replace string
 $reg_reset =~ s/0b//g;
 $reg_reset =~ s/0x//g;
 $reg_reset =~ s/[X]+/x/g;

}

## Add the final register
## Parameters for the default value 
if($sticky_flag) {
  $tp = sprintf("parameter DFLT_%-40s = {",$reg_name);
  $tp .= join(",",reverse @reg_full_def);
  $tp .= "};\n";
  push (@param_def,$tp); 
}

close($TFILE);


## Finally print all the necessary
if($opt_param) {
  print @param_addr;
  print @param_def;
}

if($opt_autover) {
	print @ports;
}

## Sub routine for help
sub help {
  print "csvparser [-dbg] [-param] [-autover] -f <csv file name> -o <output file> \n";
}

