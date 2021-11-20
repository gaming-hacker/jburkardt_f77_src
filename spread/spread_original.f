      Program SPRead
C
C ***   Program SPRead analyzes all of the sequences in a sequence data
C ***   file to identify the tyrosine residues that are most likely to
C ***   undergo tryosine sulfation as a posttranslational modification.
C ***   Subroutine DBSite reads the sequences one at a time from the data
C ***   file and finds all of the tyrosine residues in the sequences.
C ***   The subroutine returns a each potential site as a subsequence of
C ***   up to 15 residues as a character string in array ChrSeq and as
C ***   a numerical list of residue indentities in array IntSeq.
C
C ***   Subroutine Score scores each potential site in array IntSeq using
C ***   the position specific score stored in array Profil and returns
C ***   a score in array Grade that reflects how similar the site is to
C ***   a set of sites that are experimentally known to be tyrosine
C ***   sulfation sites.
C
C ***   Function Posn15 then compares the site from the data file to a
C ***   table of sites that are experimentally known to be tyrosine
C ***   sulfation sites.  This result, in variable IsPos, is used to
C ***   label the output for ROC plot analysis.  Sites that are identical
C ***   to one of the sites in array PosTbl (Positives Table) are labeled
C ***   with a plus sign, "+", and those that do not match any of the
C ***   know sites are lableled with a minus sign, "-".  This output is
C ***   used as input for the ROC plot program.
C
      Implicit None
C
C ***   Parameters defining the data structure and size
C
      Integer       BProP, EProP, MSiteP, KSiteP, NAAP
C
      Parameter   (BProP = -7, EProP = 7, MSiteP = 1000, 
     *             KSiteP = 73, NAAP = 24 )
C
C ***   Passed Variables
C
      Integer       Termnl, DbFile, LS, NSites, ns, BWin, EWin, i, k,
     *              Result, PIRfl, KeyBrd
      Integer       IntSeq( BProP:EProP, MSiteP ), TyrNum( MSiteP ),
     *              HitKnt(KSiteP)
      INTEGER       AANumb( 0:127 )
      Real*4        Profil( BProP:EProP, NAAP ), Grade( MSiteP ),
     *              Cuts
      CHARACTER*1   AAChar(24)
      Character*6   SeqAc, PosId(0:KSiteP)
      Character*10  SeqID
      Character*15  ChrSeq( MSiteP ), PosTbl(0:KSiteP)
      Logical       Fertig
C
      Integer       IsPos, Posn15
C
C
      Data  AAChar  / 'a', 'r', 'n', 'd', 'c', 'q', 'e', 'g', 'h',
     *                'i', 'l', 'k', 'm', 'f', 'p', 's', 't', 'w',
     1                'y', 'v', 'b', 'z', 'x', '-'  /
C
C                           ' '        #           *        '-' '.'
      DATA AANumb / 32 * -1,100,-1,-1,19,3*-1,3*0,200,-1,0, 24,  0,  0,    HBN
C
C                        1   2                           A   B   C   D
     *             -1, 300,302, 10 * -1, 0, -1, -1, -1,  1, 21,  5,  4,    HBN
C
C                   E    F   G   H    I  J   K   L   M   N   O   P   Q
     1              7,  14,  8,  9,  10, 0, 12, 11, 13,  3,  0, 15,  6,    HBN
C
C                   R    S    T   U    V   W   X   Y   Z             a
     2              2,  16,  17,  0,  20, 18, 23, 19, 22, 6 * -1,    1,    HBN
C
C                    b   c   d    e    f   g   h   i  j   k   l   m  n
     3              21,  5,  4,   7,  14,  8,  9, 10, 0, 12, 11, 13, 3,    HBN
C
C                   o    p   q    r    s   t   u   v   w   x   y   z
     4              0,  15,  6,   2,  16, 17,  0, 20, 18, 23, 19, 22,      HBN
     5              5 * -1   /                                             HBN
C
      Data  PosID / 'Null00', 'FICAN2', 'FICAN3', 'FIHOR3', 'FIRAB4',
     *              'FIPIG4', 'FILAM4', 'FIELE4', 'FITAP4', 'FICAP5',
     1              'LSLEU5', 'FIBIS6', 'LSLEU6', 'FIANA6', 'FISyN6',
     2              'FIBOV6', 'FIODO6', 'FIRAN6', 'FICER6', 'FIMUN6',
     3              'FIANT6', 'COB200', 'CM1416', 'SGH341', 'SGM348',
     4              'CM1413', 'F8H737', 'FAB200', 'G1H292', 'F8H738',
     5              'G1H295', 'CHIK47', 'CH1419', 'HEHU79', 'FIH444',
     6              'ITHI64', 'CAX174', 'ITHI61', 'ITHI63', 'G1H294',
     7              'F8H742', 'CCR111', 'CCH111', 'CCP110', 'AMR965',
     8              'CCFR41', 'CCRA63', 'CCHU97', 'HEHU92', 'FH1699',
     9              'GAFE12', 'GAHU87', 'GACA29', 'GARA87', 'CICIO2',
     A              'CM1417', 'FAH365', 'SGH151', 'SGR153', 'VID172',
     B              'CICIO3', 'CCTR46', 'CAX156', 'CCR113', 
     C              'CCH113', 'CCP112', 'GAR103', 'FG1683', 'CH1417',
     D              'GABO12', 'FIPE13', 'A2H484', 'GACH28', 'GAOP28' /
      Data (PosTbl(i),i=0,37)/ '               ', '------hyyddtdee',
     *      '-----hyyddtdeee', '-----ldydheeedg', '----addyddevlpd',
     1      '----aidydededgr', '----atdydeeeddr', '----atdyedeefpg',
     2      '----lsdydeeeder', '---gyldydevddnr', '---qsddyghmrf--',
     3      '--efptdydegeddr', '--eqfedyghmrf--', '--qastdyddedest',
     4      '--qfptdydegeddr', '--qfptdydegqddr', '--qhladydevdddr',
     5      '--qhladydeveddr', '--qhstdydeeeedr', '--qhstdydeveddr',
     6      '--qpsydydeeeddr', 'aaraeleyglvaeae', 'anedyedyydmpaad',
     7      'aseeepeygeeikgy', 'aseeepeygeesrsy', 'awdanedyedyydmp',
     8      'cdkntgdyyedsyed', 'daselehydpadlsp', 'degdtdlydyypeed',
     9      'dkntgdyyedsyedi', 'dtdlydyypeedteg', 'ealhdhfypdwmdf-',
     A      'eanedyeydelpakd', 'egeedddyldlekif', 'ehpaeteydslyped',
     B      'epipedayde-----', 'fadgqqdytgwmdfg', 'fdpipeeyls-----' /
      Data (PosTbl(i),i=38,73) /
     *      'feeipeeylq-----', 'gdtdlydyypeedte', 'gdyyedsyedisayl',
     1      'grrsaedyeyps---', 'grrsaeeyeyps---', 'grrsaeeyeyts---',
     2      'gteseeeysaplpkp', 'hpmrdrdyagwmdf-', 'hrindrdymgwmdf-',
     3      'hrisdrdymgwmdfg', 'ifsedddyidivdsl', 'kkedfdiydedenqs',
     4      'leeeeaaygwmdfgr', 'leeeeeaygwmdfgr', 'meeeeaaygwmdf--',
     5      'meeeeeaygwmdfgr', 'mqrmdrnyygwmdfg', 'nedyedyydmpaadd',
     6      'nneeaedydddltds', 'pmdmsddyetqqwpe', 'pvdtpddyetqqwpe',
     7      'qpyettdysneeqsq', 'qrmdrnyygwmdfgk', 'rplhdhdypgwmdf-',
     8      'rrdgqqdytgwmdfg', 'rsaedyeyps-----', 
     9      'rsaeeyeyps-----', 'rsaeeyeyts-----', 'saeeedqyn------',
     A      'sdqeeidyddtisve', 'tmeanedyeydelpa', 'veeeeaaygwmdf--',
     B      'vgqpendydtgddbt', 'vppmeedypqfgspk', 'waeeeaaygwmdf--',
     C      'wleeeeaygwmdf--'  /
C
C
C ***   Log-Odds position specific scoring matrix for tyrosine sulfation
C ***   sites.  The matrix is in units of (whole) bits.  The matrix was
C ***   computed using 44 known tyrosine sulfation sites in a variety of
C ***   peptides and proteins.  The background counts were derived from
C ***   sites around tyrosines in the same proteins and peptides which
C ***   were not found to accept tyrosine sulfation.  Pseudocounts to
C ***   fill zero cells in the table were derived from the amino acid
C ***   composition totaled over the positions from -7 to +7 amino acids
C ***   from the sulfated tyrosines.  A total of 13 pseudocounts were
C ***   added to each position (column) in the counts table.
C
C ***   The most effective range to use for identifying new sites with
C ***   this matrix is a window of from -5 (5 amino acids N-terminal
C ***   to the sulfated tyrosine) to +5 (5 amino acids C-terminal).
C
C ***    Alanine
      Data (Profil(i,1), i = -7,7,1)/  0.266,  0.839, -0.662,  0.044,
     *                -0.378, -0.956, -0.609,  0.000, -1.199, -0.882,
     1                -0.880, -0.442,  0.665,  0.262, -0.818 /
C
C ***   Arginine
      Data (Profil(i,2), i = -7,7,1)/ -0.439, -0.005,  0.057, -0.568,
     *                -1.106, -0.444, -3.063,  0.000, -3.067, -2.790,
     1                -3.107,  0.291, -2.485, -2.466,  0.318 /
C
C ***   Asparagine
      Data (Profil(i,3), i = -7,7,1)/  0.936,  0.456,  0.676, -0.298,
     *                -0.044, -0.234, -0.617,  0.000, -1.307, -0.596,
     1                -1.559, -2.502, -0.718, -0.141, -2.580 /
C
C ***   Aspartic Acid
      Data (Profil(i,4), i = -7,7,1)/  1.124,  0.915,  0.965,  1.469,
     *                 2.111,  2.373,  3.652,  0.000,  2.444,  2.369,
     1                 1.130,  1.532,  2.973,  1.493,  1.667 /
C
C ***   Cysteine
      Data (Profil(i,5), i = -7,7,1)/ -0.577, -5.486, -5.010, -5.344,
     *                -5.064, -5.361, -4.690,  0.000, -4.694, -4.668,
     1                -5.285, -4.978, -4.559, -5.893, -5.402 /
C
C ***   Glutamine
      Data (Profil(i,6), i = -7,7,1)/  0.196, -0.692,  0.408, -1.330,
     *                -1.281,  0.223, -0.846,  0.000, -1.847,  0.068,
     1                -0.634, -0.846, -1.088, -1.069, -0.710 /
C
C ***   Glutamic Acid
      Data (Profil(i,7), i = -7,7,1)/  1.249,  1.716,  1.013,  1.925,
     *                 2.619,  2.233,  1.842,  0.000,  2.380,  2.416,
     1                 1.128,  1.473,  0.498,  1.036,  1.060 /
C
C ***   Glycine
      Data (Profil(i,8), i = -7,7,1)/  0.945, -0.262,  0.008, -0.136,
     *                -0.903, -1.118, -2.518,  0.000,  0.802,  0.675,
     1                -0.950, -1.236, -1.766, -0.081,  0.568 /
C
C ***   Histidine
      Data (Profil(i,9), i = -7,7,1)/  0.697, -0.708, -2.151,  0.958,
     *                -1.663,  0.573, -0.015,  0.000, -1.684,  0.656,
     1                -2.952, -3.386, -2.646, -2.464, -3.049 /
C
C ***   Isoleucine
      Data (Profil(i,10),i = -7,7,1)/ -0.678, -2.243,  0.622, -0.895,
     *                -3.245, -1.014, -0.938,  0.000, -1.316, -2.930,
     1                 0.100, -0.300, -2.821,  0.057, -0.194 /
C
C ***   Leucine
      Data (Profil(i,11),i = -7,7,1)/ -3.472, -3.224, -1.043, -2.124,
     *                -0.607, -0.482, -2.127,  0.000, -1.015, -2.185,
     1                -0.060, -0.329, -1.292, -2.800, -0.874 /
C
C ***   Lysine
      Data (Profil(i,12),i = -7,7,1)/ -1.483, -0.026, -1.817, -3.789,
     *                -3.732, -4.226, -3.659,  0.000, -3.468, -4.036,
     1                -3.730, -3.006, -0.245, -0.553,  0.059 /
C
C ***   Methionine
      Data (Profil(i,13),i = -7,7,1)/  1.046,  0.866,  0.967,  2.185,
     *                -0.260, -1.391, -1.205,  0.000, -0.028, -1.290,
     1                 1.846,  3.517, -1.782,  0.126, -0.038 /
C
C ***   Phenlyalanine
      Data (Profil(i,14),i = -7,7,1)/ -0.341, -0.750, -2.652, -1.285,
     *                -0.473, -1.222, -1.000,  0.000, -3.323, -2.144,
     1                -0.198, -2.422,  0.505,  1.871, -0.062 /
C
C ***   Proline
      Data (Profil(i,15),i = -7,7,1)/ -0.893,  0.441,  0.006, -0.021,
     *                 0.149, -0.264, -1.625,  0.000,  0.814, -0.132,
     1                -0.214,  1.181, -0.333,  0.700,  1.046 /
C
C ***   Serine
      Data (Profil(i,16),i = -7,7,1)/ -0.550, -0.343, -0.427,  0.339,
     *                -1.505, -3.229, -0.964,  0.000, -0.502, -0.402,
     1                -1.299, -0.734, -0.293, -0.080, -0.258 /
C
C ***   Threonine
      Data (Profil(i,17),i = -7,7,1)/ -0.772, -0.367, -1.024, -1.051,
     *                -0.023,  0.978, -2.630,  0.000, -0.199,  0.132,
     1                -0.344, -1.285, -0.153, -1.303, -0.487 /
C
C  ***  Tryptophan
      Data (Profil(i,18),i = -7,7,1)/  0.188,  1.750,  0.109, -1.323,
     *                -2.516, -2.038, -2.317,  0.000, -2.891,  0.585,
     1                 3.502, -1.264,  0.491, -1.217, -2.931 /
C
C ***   Tyrosine
      Data (Profil(i,19),i = -7,7,1)/ -0.391, -1.327,  0.702,  1.019,
     *                 1.105,  0.405,  0.725,  0.000,  1.000,  0.400,
     1                 0.735,  1.743, -0.054,  0.110,  1.287 /
C
C ***   Valine
      Data (Profil(i,20),i = -7,7,1)/  0.027, -3.830, -3.805, -4.387,
     *                -4.162, -4.968, -4.430,  0.000, -4.648, -4.483,
     1                 0.137, -2.035, -3.853, -1.702, -4.266 /
C
C ***   Asartic Acid or Asparagine  --  ambiguous (Asx)
      Data (Profil(i,21),i = -7,7,1)/  1.070,  0.765,  0.886,  0.803,
     *                 1.042,  1.036,  1.443,  0.000,  0.416,  1.068,
     1                 0.487, -0.033,  1.072,  0.817, -0.245 /
C
C ***   Glutamic Acid or Glutamine  --  ambiguous (Glx)
      Data (Profil(i,22),i = -7,7,1)/  0.827,  0.427,  0.853,  0.306,
     *                 0.381,  1.641,  0.599,  0.000,  0.535,  1.204,
     *                 0.443,  0.489, -0.193,  0.096,  0.370 /
C
C ***   Unknown or completely ambiguous amino acid  --  X
      Data (Profil(i,23),i = -7,7,1)/ -0.420, -0.559, -0.468, -0.853,
     *                -0.929, -1.249, -1.415,  0.000, -1.178, -1.107,
     1                -0.716, -0.822, -0.750, -0.593, -0.666 /
C
C ***   Gap (beyond the N-terminal or C-terminal end of the sequence).
      Data (Profil(i,24),i = -7,7,1) /   15 * 0.000   /
C
C
C    1 Format( ' ', A10, 2X, A6, ' Site #', I3, ' Tyr #', I4, 2X, A15,
C     *        2X, F9.3 )
C    3 Format( '       This site matches the known sulfation site:  ',
C     *          A6, 2X, A15 )
C    4 Format(' ', 15F5.2 )
C
    5 Format( A1, 1X, A10, 1X, A6, I6, F9.3:, 2X, A6)
    6 Format( ' ', A6, I6 )
    7 Format(' The number of sites in the database that were found',
     *  /' to be identical to the specified known positive site.', / )
    8 Format(//' Please enter a cutoff score - sequences for all',
     *         ' sites with scores',
     1        /' greater than or equal to this will be reported.'/)
    9 Format( F )
   10 Format( A1, 1X, A10, 1X, A6, I6, F9.3, 2X, A6, 2X, A15 )
   11 Format( A1, 1X, A10, 1X, A6, I6, F9.3, 10X, A15 )
   12 Format( '>F1;',A10, /'  ', A1, '  SeqID: ', A10,'  Ac Num: ',A6,
     *                     '  Tyr at:',I6, '  Score =' F9.3,
     1                    / A15 '*' )
C
C
      Termnl = 6
      KeyBrd = 5
      DbFile = 10
      BWin = -5
      EWin = 5
      Result = 2
      PIRfl = 3
C
      Write( Termnl, 8 )
      Read( KeyBrd, 9 )   Cuts
C
C      Do 90 i = 1, 23
C         Write( Termnl, 4 )  ( Profil(k,i), k = BProP, EProP, 1 )
C   90    continue
C
      Open( Unit = DBFile, file = 'metazoa.SEQ', Status = 'Old' )
C
      Open( Unit = Result, File = 'metazoa.score', status = 'new',
     *      Access = 'Sequential', Form = 'Formatted',
     *      CarriageControl = 'list' )
C
      Open( Unit = PIRfl, File = 'metazoa.pir', status = 'new',
     *      Access = 'Sequential', Form = 'Formatted',
     *      CarriageControl = 'list' )
C
  150 NSites = 0
      SeqID = '          '
      SeqAc = '      '
      Call DbSite( Termnl, DbFile, LS, BProP, EProP, NSites,
     *             MSiteP, IntSeq, ChrSeq, TyrNum, AANumb,
     1             AAChar, SeqID, SeqAc, Fertig )
      If( Fertig )    GoTo 1000
C
C ***   Process the sites
C
      Call Score( BProP, EProP, NAAP, MSiteP, NSites, Profil, IntSeq,
     *            Grade, BWin, EWin )
      Do 200 ns = 1, NSites
C         Write( Termnl, 1 )  SeqID, SeqAc, ns, TyrNum(ns), ChrSeq(ns),
C     *                       Grade(ns)
         IsPos = Posn15( KSiteP, ChrSeq(ns), PosTbl )
         If( IsPos .gt. 0 )    Then
            Write(Result,10)  '+', SeqID, SeqAc, TyrNum(ns),
     *                             Grade(ns), PosId(IsPos),
     1                             ChrSeq(ns)
            Write( PIRfl, 12 )  SeqID, '+', SeqID, SeqAc, TyrNum(ns),
     *                          Grade(ns), ChrSeq(ns)
            HitKnt(IsPos) = HitKnt(IsPos) + 1
         Else
            If( Grade(ns) .ge. Cuts )    Then
               Write( Result, 11 )  '-', SeqID, SeqAc, TyrNum(ns),
     *                                   Grade(ns), ChrSeq(ns)
               Write( PIRfl, 12 )  SeqId, '-', SeqID,SeqAc,TyrNum(ns),       
     *                                  Grade(ns), ChrSeq(ns)
            Else
               Write(Result,5)  '-', SeqID, SeqAc,TyrNum(ns),Grade(ns)
            EndIf
         EndIf
  200    continue
      GoTo 150
C
C
 1000 Close( Unit = DbFile, status = 'Keep' )
      Close( Unit = Result, status = 'Keep' )
      Close( Unit = PIRfl,  status = 'Keep' )
C
      Open( Unit = Result, File = 'identical.sites', status = 'new',
     *      Access = 'Sequential', Form = 'Formatted',
     *      CarriageControl = 'list' )
      Write( Result, 7 )
      Do 1100 k = 1, KSiteP
         Write( Result, 6 )  PosId(k), HitKnt(k)
 1100    continue
      Close( Unit = Result, status = 'Keep' )
C
      Stop ' Swiss-Prot sequences scored.'
      End
C
C
      Subroutine DbSite( Termnl, DbFile, LS, BProP, EProP, NSites,
     *                   MSiteP, IntSeq, ChrSeq, TyrNum, AANumb,
     1                   AAChar, SeqID, SeqAc, Fertig )
C
      IMPLICIT NONE
C
C ***   Passed Variables
C
      Integer       Termnl, DbFile, LS, BProP, EProP, NSites,
     *              MSiteP
      Integer       IntSeq( BProP:EProP, MSiteP ), TyrNum( MSiteP )
      INTEGER       AANumb( 0:127 )
      CHARACTER*1   AAChar(24)
      Character*6   SeqAc
      Character*10  SeqID
      Character*15  ChrSeq( MSiteP )
      Logical       Fertig
C
C ***   Local Variable Declarations
C
      Integer       l, lb, le, nl, NLines, wp, Wbgn, Wend, j, k, i
      Integer       seq( 25000 )
      Character*1   LSeq( 25000 )
      Character*80  Line
C
    1 Format( A80 )
    2 Format( I6 )
    3 Format(/' Accession line is missing after the following line:',
     *       /' :', A60 )
    4 Format(/, 9X, 5( 1X, 10A1: ) )
C
C
C ***   Read file until an "ID" (identifier) line is detected and get
C ***   the 10 character identifier and the length
C
  100 Read( DbFile, 1, End = 1000 )  Line
         Fertig = .False.
         If( Line(1:2) .eq. 'ID' )    Then
            SeqId = Line(6:15)
            Read( Line(40:45), 2 )  LS
         Else
            GoTo 100
         EndIf
C
C ***   The next line should be the "AC" (accession number) line.  Save
C ***   the accession number since it is the only guarenteed means of
C ***   locating and identifying the sequence.
C
      Read( DbFile, 1 )  Line
      If( Line(1:2) .eq. 'AC' )    Then
         SeqAc = Line( 6:12 )
      Else
         Write( Termnl, 3 )  Line(1:60)
         Stop ' Bad Accession Number Line in Database.'
      EndIf
C
C ***   Skip lines (should be only one) until we get to the line marking
C ***   the start of the sequence data.
C
  110 Read( DbFile, 1 )  Line
         If( Index( Line, '..' ) .gt. 0   .and.
     *       Index( Line, 'Type: P' ) .gt. 0 )    Then
         Else
            GoTo 110
         EndIf
C
C ***   Now read the sequence
C
      NLines = ( LS + 49 ) / 50
      lb = -49
      Do 120 nl = 1, NLines
         lb = lb + 50
         le = lb + 49
         If( le .gt. LS )    le = LS
         Read( DbFile, 4 )   ( lseq(l), l = lb, le, 1 )
  120    continue
C
C ***   Convert the sequence characters to a numerical equivalent
C
      Do 150 L = 1, LS
         seq(l) = AANumb( ichar( lseq( l ) ) )
  150    continue
C
C ***   Find the potential tyrosine sulfation sites in the sequence
C
      DO 500 L = 1, LS
         If( seq(l) .eq. 19 )    Then
            NSites = NSites + 1
            TyrNum( NSites ) = l
            ChrSeq( NSites ) = '---------------'
            Wbgn = L + BProP
            j = BProP
            If( Wbgn .LT. 1 )    Then
               j = BProP -( WBgn - 1 )
               Wbgn = 1
            Else
            EndIf
            k = j + 7
            Wend = L + EProP
            If( Wend .GT. LS )    Wend = LS
            Do 190 i = BProp, EProp, 1
               IntSeq( i, NSites ) = 24
  190          continue
            Do 200 wp = Wbgn, Wend, 1
               k = k + 1
               ChrSeq( NSites )( k:k ) = AAChar( seq( wp ) )
               IntSeq( j, NSites ) = seq( wp )
               j = j + 1
  200          continue
C
         Else
         END IF
  500    CONTINUE
      Return
C
 1000 Fertig = .True.
      RETURN
      END
C
C
C
      INTEGER FUNCTION POSN15( SIZE, STRING, TABLE )
C
C ****************************************************************************
C **   Copyright  --  Pittsburgh Supercomputing Center  --  December 1988   **
C ********************   Coded by Hugh B. Nicholas   *************************
C
C
C ***   The functions, STDPSN is a binary search of the table STDPOS .
C ***   search algorithm is an implementation of Knuth's algorithm U
C ***   (vol 3., ch. 6.2.1, p 411; see vol 1., ch. 1.2.4, p 37 for
C ***   details of the notation).
C
C ***   INDEX = The current position in the search table.
C ***   RATE  = The change in INDEX if a match is not found on the current
C ***         cycle.  If RATE becomes zero the value passed to the
C ***         function is not in the search table.
C
C
      INTEGER         RATE, INDEX, SIZE
      CHARACTER*15    STRING, TABLE( 0 : SIZE )
C
C
      RATE = SIZE  / 2
      INDEX = ( SIZE + 1 ) / 2
  200 IF( LLT( STRING, TABLE( INDEX ) ) )    THEN
         INDEX = INDEX - ( ( RATE + 1 ) / 2 )
      ELSE IF( LGT( STRING, TABLE( INDEX ) ) )    THEN
         INDEX = INDEX + ( ( RATE + 1 ) / 2 )
      ELSE IF( STRING .EQ. TABLE( INDEX ) )    THEN
         Posn15 = INDEX
         RETURN
      END IF
      IF( RATE .EQ. 0 )    THEN
         Posn15 = 0
         RETURN
      ELSE
      END IF
      RATE = RATE / 2
      GO TO 200
C
      END
C
C
      Subroutine Score( BProP, EProP, NAAP, MaxSqP, NSites, Profil,
     *                  Seq, Grade, BWin, EWin )
C
      Implicit None
C
      Integer     BProP, EProP, NAAP, MaxSqP, NSites, BWin, EWin
      Integer     Seq( BProP:EProP, MaxSqP )
      Real*4      Profil( BProP:EProP, NAAP ), Grade( MaxSqP )
C
C ***   Local Variables
C
      Integer     pos, ns
C
      Do 100 ns = 1, NSites, 1
         Grade( ns ) = 0.0
         Do 90 pos = BWin, EWin, 1
            Grade( ns ) = Grade(ns) + Profil( pos, Seq(pos,ns) )
   90       continue
  100    continue
C
      Return
      End

