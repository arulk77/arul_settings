#!/usr/bin/env perl
use strict;

use Time::localtime;
use File::Basename;
use Cwd;
use Cwd 'abs_path';
use FindBin;
use Text::CSV_PP;
use Text::ParseWords;

my $debugFile = "csvparser.debug";
my $opt_debug=0;
my $opt_param=0;
my $opt_autover=0;
my $arg_string;
my $arg;
my $csv_file="x.csv";
my @fields;
my $temp="x.csv.tmp";

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
   elsif ($ARGV[$i] eq "-o")                  { $temp = $ARGV[++$i]; }
   else  {die "Cannot decode option $ARGV[$i]"; }
}

# initialize special files
if ($opt_debug) { open (DBG,">$debugFile") or die "Can't append to $debugFile\n\n"; }


my $first=1;
my $prv_reg_addr= "";

## Decode the registers 
## Open the temporary file
open (my $TFILE, '<', $temp) or die "Cannot open file $temp in read mode";
my $lno = 0;
my @row;
my ($reg_name,$reg_addr,$reg_field);
my ($reg_msb,$reg_lsb,$reg_attr,$reg_desc);
my ($reg_rst,$hval,$reg_offset);
my $reg_jack = 1;
my $reg_gen4 = 1;

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

  $reg_addr    = $row[0]; 
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
	}

  ## Triage the script 
  my $bits = "[$reg_msb:$reg_lsb]";
  if($reg_msb eq $reg_lsb) {
    $bits = "[$reg_msb]";
  }

  ## If there are exception lines do not process 
  if ($row[0] =~ /Offset/) {
    if($opt_debug) {print DBG "$lno : Found exception \n";} next;
  }
  
  ## Flag to check if the register name is hit
  if (!($reg_addr eq $prv_reg_addr)) {
    $prv_reg_addr = $reg_addr; 
    $reg_name     = $reg_field;
    $reg_offset   = $reg_addr;

    if($opt_debug) {print DBG "$lno : Found new register $reg_name \n";}

    ## Print register offset 
    if($opt_debug) {print DBG "$lno : register name $reg_name with offset value register value is $reg_offset\n";}

		## Replace any special space with _ character
	  $reg_name =~ s/(\s|\[|\])/_/g;	
  } 

 ## Triage if it is reserved field
 if($reg_field =~ /reserved/ || $reg_field =~ /Reser/) {
   $reg_acc = "RsvdP";
 }

 ## Search and replace string
 $reg_rst =~ s/0b//g;
 $reg_rst =~ s/0x//g;
 $reg_rst =~ s/[X]+/x/g;

 if($reg_acc =~ /Rsvd/g) {
   $reg_rst = 0;
 }

  ## Print each register specific values 
  print "$bits,$reg_rst,$reg_acc,yes,Level01,$reg_field,\"$reg_desc\",\n";
}

close($TFILE);

## Sub routine for help
sub help {
  print "csvparser [-dbg] [-param] [-autover] -f <csv file name> -o <output file> \n";
}


