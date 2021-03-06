
$ clear

$ PS1='$ '
$ whoami
mickles
$ pwd
/Volumes/mickles
$ ls
AppleScripts		Serial Box 01-2016.sb2
Application Libraries	Sites
Applications		Storage
Desktop 		To Sort Desktop
Documents		Virtual Machines
Downloads		edid-envision.txt
Library 		gbdinit
MobileAutoMount.sh	git-completion.bash
Movies			gptsync
Music			iPhoneAutoMount.sh
My Cloud		linuxchrome.sh
Pictures		mickley.pem
Public			shsh
RStats			tvrenamer.preferences
$ ls -F
AppleScripts/		Serial Box 01-2016.sb2
Application Libraries/	Sites/
Applications/		Storage/
Desktop/		To Sort Desktop/
Documents/		Virtual Machines/
Downloads/		edid-envision.txt
Library/		gbdinit*
MobileAutoMount.sh*	git-completion.bash
Movies/ 		gptsync*
Music/			iPhoneAutoMount.sh*
My Cloud/		linuxchrome.sh*
Pictures/		mickley.pem
Public/ 		shsh/
RStats/ 		tvrenamer.preferences
$ man ls

LS(1)			  BSD General Commands Manual			 LS(1)

NAME
     ls -- list directory contents

SYNOPSIS
     ls [-ABCFGHLOPRSTUW@abcdefghiklmnopqrstuwx1]
	[file ...]

DESCRIPTION
     For each operand that names a file of a type other
     than directory, ls displays its name as well as any
     requested, associated information.  For each operand
     that names a file of type directory, ls displays the
$ clear
$ ls -F Desktop
SWC Software/		data-shell/
SWC-R/			shell-novice-data.zip
$ ls -F Desktop/data-shell/
Desktop/		notes.txt
creatures/		pizza.cfg
data/			solar.pdf
molecules/		writing/
north-pacific-gyre/
$ cd Desktop/
$ pwd
/Volumes/mickles/Desktop
$ cd data-shell
$ pwd
/Volumes/mickles/Desktop/data-shell
$ ls -F
Desktop/		notes.txt
creatures/		pizza.cfg
data/			solar.pdf
molecules/		writing/
north-pacific-gyre/
$ cd ..
$ pwd
/Volumes/mickles/Desktop
$ ls -Fa
./			SWC-R/
../			data-shell/
.DS_Store		shell-novice-data.zip
SWC Software/
$ ls .
SWC Software		data-shell
SWC-R			shell-novice-data.zip
$ ls ..
AppleScripts		Serial Box 01-2016.sb2
Application Libraries	Sites
Applications		Storage
Desktop 		To Sort Desktop
Documents		Virtual Machines
Downloads		edid-envision.txt
Library 		gbdinit
MobileAutoMount.sh	git-completion.bash
Movies			gptsync
Music			iPhoneAutoMount.sh
My Cloud		linuxchrome.sh
Pictures		mickley.pem
Public			shsh
RStats			tvrenamer.preferences
$ ls -Fa
./			SWC-R/
../			data-shell/
.DS_Store		shell-novice-data.zip
SWC Software/
$ ls -F -a
./			SWC-R/
../			data-shell/
.DS_Store		shell-novice-data.zip
SWC Software/
$ pwd
/Volumes/mickles/Desktop
$ cd
$ pwd
/Volumes/mickles
$ clear
$ cd Desktop/data-shell/data
$ cd Desk
bash: cd: Desk: No such file or directory
$ cd /Volumes/mickles/Desktop/data-shell
$ pwd
/Volumes/mickles/Desktop/data-shell
$ cd ~
$ pwd
/Volumes/mickles
$ cd
$ pwd
/Volumes/mickles
$ cd ~/Desktop
$ pwd
/Volumes/mickles/Desktop
$ cd -
/Volumes/mickles
$ pwd
/Volumes/mickles
$ pwd
/Volumes/mickles
$ cd Desktop/data-shell/
$ pwd
/Volumes/mickles/Desktop/data-shell
$ ls
Desktop 		notes.txt
creatures		pizza.cfg
data			solar.pdf
molecules		writing
north-pacific-gyre
$ ls north-pacific-gyre/
2012-07-03
$ cd ~/Desktop/data-shell/
$ pwd
/Volumes/mickles/Desktop/data-shell
$ ls -F
Desktop/		notes.txt
creatures/		pizza.cfg
data/			solar.pdf
molecules/		writing/
north-pacific-gyre/
$ clear
$ mkdir thesis
$ ls -F
Desktop/		notes.txt
creatures/		pizza.cfg
data/			solar.pdf
molecules/		thesis/
north-pacific-gyre/	writing/
$ ls thesis directory
ls: directory: No such file or directory
thesis:
$ ls -thesis
total 64
33397578  0 thesis		33389484  0 north-pacific-gyre
33389475  0 Desktop		33389505  8 notes.txt
33389312  0 creatures		33389506  8 pizza.cfg
33389315  0 data		33389507 48 solar.pdf
33389477  0 molecules		33389508  0 writing
$ cd thesis
$ clear
$ pwd
/Volumes/mickles/Desktop/data-shell/thesis
$ ls -F
$ ls -F -a
./	../
$ nano draft.txt
$ rm draft.txt
$ ls
$ ls -F -a
./	../
$ nano.draft.txt
$ cd ..
$ pwd
/Volumes/mickles/Desktop/data-shell
$ rm thesis
rm: thesis: is a directory
$ ls -F -a
./			north-pacific-gyre/
../			notes.txt
.bash_profile		pizza.cfg
Desktop/		solar.pdf
creatures/		thesis/
data/			writing/
molecules/
$ rm -r -itthesis
examine files in directory thesis? y
remove thesis/draft.txt? yy
remove thesis? y
lsr-F--a-i thesis
./			molecules/
../			north-pacific-gyre/
.bash_profile		notes.txt
Desktop/		pizza.cfg
creatures/		solar.pdf
data/			writing/
$ clear
$ pwd
/Volumes/mickles/Desktop/data-shell
$ mkdir thesis
lse-Fi-athesis
./			north-pacific-gyre/
../			notes.txt
.bash_profile		pizza.cfg
Desktop/		solar.pdf
creatures/		thesis/
data/			writing/
molecules/
$ nano thesis/draft.txt
$ mv thesis/draft.txt thesis/quotes.txt
$ ls thesis
quotes.txt
$ mv thesis/quotes.txt ./quotes.txt
$ ls -F -a
./			north-pacific-gyre/
../			notes.txt
.bash_profile		pizza.cfg
Desktop/		quotes.txt
creatures/		solar.pdf
data/			thesis/
molecules/		writing/
cleare-as/quotes.txt .
pwd-Fe-as/quotes.txt .
/Volumes/mickles/Desktop/data-shell
$ ls -F -a
./			north-pacific-gyre/
../			notes.txt
.bash_profile		pizza.cfg
Desktop/		quotes.txt
creatures/		solar.pdf
data/			thesis/
molecules/		writing/
$ ls quotes.txt
quotes.txt
$ ls quotes.txt solar.pdf
quotes.txt	solar.pdf
$ ls quotes.txt solar.pdf james.txt
ls: james.txt: No such file or directory
quotes.txt	solar.pdf
$ clear
$ cp quotes.txt thesis/quotations.txt
$ ls quotes.txt thesis/quotations.txt
quotes.txt		thesis/quotations.txt
$ ls -F -a
./			north-pacific-gyre/
../			notes.txt
.bash_profile		pizza.cfg
Desktop/		quotes.txt
creatures/		solar.pdf
data/			thesis/
molecules/		writing/
$ ls -F -a thesis
./		../		quotations.txt
$ rm quotes.txt
lsr-Fq-atthesis
./			north-pacific-gyre/
../			notes.txt
.bash_profile		pizza.cfg
Desktop/		solar.pdf
creatures/		thesis/
data/			writing/
molecules/
lsl-Fo-asthesis
./		../		quotations.txt
$ clear
$ pwd
/Volumes/mickles/Desktop/data-shell
$ ls -F
Desktop/		notes.txt
creatures/		pizza.cfg
data/			solar.pdf
molecules/		thesis/
north-pacific-gyre/	writing/
$ ls molecules
cubane.pdb	methane.pdb	pentane.pdb
ethane.pdb	octane.pdb	propane.pdb
$ wc cubane.pdb
wc: cubane.pdb: open: No such file or directory
$ cd molecules
wcccubane.pdbs
      20     156    1158 cubane.pdb
$ wc *.pdb
      20     156    1158 cubane.pdb
      12      84     622 ethane.pdb
       9      57     422 methane.pdb
      30     246    1828 octane.pdb
      21     165    1226 pentane.pdb
      15     111     825 propane.pdb
     107     819    6081 total
$ wc p*.pdb
      21     165    1226 pentane.pdb
      15     111     825 propane.pdb
      36     276    2051 total
$ wc ??hane.p*
      12      84     622 ethane.pdb
$ wc *.pdf
wc: *.pdf: open: No such file or directory
$ ls
cubane.pdb	methane.pdb	pentane.pdb
ethane.pdb	octane.pdb	propane.pdb
$ wc pentane.pdb propane.pdb
      21     165    1226 pentane.pdb
      15     111     825 propane.pdb
      36     276    2051 total
p*.pdbane.p*.pdb propane.pdb
      21     165    1226 pentane.pdb
      15     111     825 propane.pdb
      36     276    2051 total
$ ls *t*ane.pdb
ethane.pdb	methane.pdb	octane.pdb	pentane.pdb
$ ls *t?ne.*
octane.pdb	pentane.pdb
$
$ ls *t??ne.pdb
ethane.pdb	methane.pdb
$ wc -l *.pdb
      20 cubane.pdb
      12 ethane.pdb
       9 methane.pdb
      30 octane.pdb
      21 pentane.pdb
      15 propane.pdb
     107 total
$ wc w**.pdbb
     156 cubane.pdb
      84 ethane.pdb
      57 methane.pdb
     246 octane.pdb
     165 pentane.pdb
     111 propane.pdb
     819 total
$ wc -l *.pdb > lengths.txt
$ ls
cubane.pdb	lengths.txt	octane.pdb	propane.pdb
ethane.pdb	methane.pdb	pentane.pdb
$ clear
$ cat lengths.txt
      20 cubane.pdb
      12 ethane.pdb
       9 methane.pdb
      30 octane.pdb
      21 pentane.pdb
      15 propane.pdb
     107 total
$ less lengths.txt
      20 cubane.pdb
      12 ethane.pdb
       9 methane.pdb
      30 octane.pdb
      21 pentane.pdb
      15 propane.pdb
     107 total
$esorts-nxlengths.txt
       9 methane.pdb
      12 ethane.pdb
      15 propane.pdb
      20 cubane.pdb
      21 pentane.pdb
      30 octane.pdb
     107 total
$ sort -n lengths.txt > sorted_lengths.txt
$ ls -F
cubane.pdb		octane.pdb
ethane.pdb		pentane.pdb
lengths.txt		propane.pdb
methane.pdb		sorted_lengths.txt
$ cat sorted_lengths.txt
       9 methane.pdb
      12 ethane.pdb
      15 propane.pdb
      20 cubane.pdb
      21 pentane.pdb
      30 octane.pdb
     107 total
$ cat lengths.txt
      20 cubane.pdb
      12 ethane.pdb
       9 methane.pdb
      30 octane.pdb
      21 pentane.pdb
      15 propane.pdb
     107 total
$ head -n 1 sorted_lengths.txt
       9 methane.pdb
$ head -n1@3sorted_lengths.txt
       9 methane.pdb
      12 ethane.pdb
      15 propane.pdb
$ ls -F
cubane.pdb		octane.pdb
ethane.pdb		pentane.pdb
lengths.txt		propane.pdb
methane.pdb		sorted_lengths.txt
$ sort -n lengths.txt | head -n 1
       9 methane.pdb
$ wc -l *.pdb
      20 cubane.pdb
      12 ethane.pdb
       9 methane.pdb
      30 octane.pdb
      21 pentane.pdb
      15 propane.pdb
     107 total
$ wc -l *.pdb | sort -n
       9 methane.pdb
      12 ethane.pdb
      15 propane.pdb
      20 cubane.pdb
      21 pentane.pdb
      30 octane.pdb
     107 total
$ wc -l *.pdb | sort -n | head -n 1
       9 methane.pdb
$ wc -l * | sort -n | head -3
       7 lengths.txt
       7 sorted_lengths.txt
       9 methane.pdb
$ wc -l * | sort -n | head 1-3
head: 1-3: No such file or directory
$ ls -F
cubane.pdb		octane.pdb
ethane.pdb		pentane.pdb
lengths.txt		propane.pdb
methane.pdb		sorted_lengths.txt
$ cd ..
$ pwd
/Volumes/mickles/Desktop/data-shell
$ clear
$ cd north-pacific-gyre/2012-07-03/
$ ls -F
NENE01729A.txt	NENE01812A.txt	NENE01978B.txt	NENE02043A.txt
NENE01729B.txt	NENE01843A.txt	NENE02018B.txt	NENE02043B.txt
NENE01736A.txt	NENE01843B.txt	NENE02040A.txt	goodiff
NENE01751A.txt	NENE01971Z.txt	NENE02040B.txt	goostats
NENE01751B.txt	NENE01978A.txt	NENE02040Z.txt
$ wc -l *.txt | sort -n | head -n 5
     240 NENE02018B.txt
     300 NENE01729A.txt
     300 NENE01729B.txt
     300 NENE01736A.txt
     300 NENE01751A.txt
$ wc -l *.txt | sort -n | head -n 5
     300 NENE02040B.txt
     300 NENE02040Z.txt
     300 NENE02043A.txt
     300 NENE02043B.txt
    5040 total
$ ls *Z.txt
NENE01971Z.txt	NENE02040Z.txt
$ echo hello
hello
$ echo hello
hello
$ echo hello >> testfile02.txt
$ cat testfile02.txt
hello
echothellof>>etestfile02.txt
catctestfile02.txtstfile02.txt
hello
hello
echothellof>>testfile02.txtt
catctestfile02.txtiksheet > testfile02.txt
Nora an Pariksheet
$ pwd
/Volumes/mickles/Desktop/data-shell/north-pacific-gyre/2012-07-03
$ cd ~/Desktop/data-shell/
$ pwd
/Volumes/mickles/Desktop/data-shell
$ clear
$ cd creatures/
$ ls -F
basilisk.dat	unicorn.dat
$ for filename in basilisk.dat unicorn.dat
>
$ for filename in basilisk.dat unicorn.dat
> do
> head -n 3 $filename
> done
COMMON NAME: basilisk
CLASSIFICATION: basiliscus vulgaris
UPDATED: 1745-05-02
COMMON NAME: unicorn
CLASSIFICATION: equus monoceros
UPDATED: 1738-11-24
$ for x in *.datisk.
> do
>    head -n 3 $x
> done
COMMON NAME: basilisk
CLASSIFICATION: basiliscus vulgaris
UPDATED: 1745-05-02
COMMON NAME: unicorn
CLASSIFICATION: equus monoceros
UPDATED: 1738-11-24
$ cd ../north-pacific-gyre/2012-07-03/
$ ls -F
NENE01729A.txt	NENE01812A.txt	NENE01978B.txt	NENE02043A.txt
NENE01729B.txt	NENE01843A.txt	NENE02018B.txt	NENE02043B.txt
NENE01736A.txt	NENE01843B.txt	NENE02040A.txt	goodiff
NENE01751A.txt	NENE01971Z.txt	NENE02040B.txt	goostats
NENE01751B.txt	NENE01978A.txt	NENE02040Z.txt	testfile02.txt
$ for datafile in NENE*.txt
> do
>    echo $datafile
>    bash goostats $datafile
> done

NENE01729A.txt
goostats: line 11: $2: ambiguous redirect
NENE01729B.txt
goostats: line 11: $2: ambiguous redirect
NENE01736A.txt
goostats: line 11: $2: ambiguous redirect
NENE01751A.txt
goostats: line 11: $2: ambiguous redirect
NENE01751B.txt
goostats: line 11: $2: ambiguous redirect
NENE01812A.txt
goostats: line 11: $2: ambiguous redirect
NENE01843A.txt
goostats: line 11: $2: ambiguous redirect
NENE01843B.txt
goostats: line 11: $2: ambiguous redirect
NENE01971Z.txt
goostats: line 11: $2: ambiguous redirect
NENE01978A.txt
goostats: line 11: $2: ambiguous redirect
NENE01978B.txt
goostats: line 11: $2: ambiguous redirect
NENE02018B.txt
goostats: line 11: $2: ambiguous redirect
NENE02040A.txt
goostats: line 11: $2: ambiguous redirect
NENE02040B.txt
goostats: line 11: $2: ambiguous redirect
NENE02040Z.txt
goostats: line 11: $2: ambiguous redirect
NENE02043A.txt
goostats: line 11: $2: ambiguous redirect
NENE02043B.txt
