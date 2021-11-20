      program main

c*********************************************************************72
c
cc MAIN is the main program for TOMS743_PRB.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 June 2014
c
c  Author:
c
c    Original FORTRAN77 version by Andrew Barry, S. J. Barry, 
c    Patricia Culligan-Hensley.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Andrew Barry, S. J. Barry, Patricia Culligan-Hensley,
c    Algorithm 743: WAPR - A Fortran routine for calculating real 
c    values of the W-function,
c    ACM Transactions on Mathematical Software,
c    Volume 21, Number 2, June 1995, pages 172-181.
c
      implicit none

      double precision dx
      integer n
      integer nbits
      double precision xmax
      double precision xmin

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TOMS743_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the TOMS743 library.'

      call nbits_compute ( nbits )
      write ( *, '(a)' ) ' '
      write ( *, '(a,i4)' ) 
     &  '  Number of bits in mantissa - 1 = ', nbits

      call test01 ( nbits )

      dx = + 1.0D-09
      n = 10
      call test02 ( nbits, dx, n )

      xmin = 0.0D+00
      xmax = 1.0D+20
      n = 20
      call test03 ( nbits, xmin, xmax, n )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TOMS743_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      stop
      end
      subroutine test01 ( nbits )

c*********************************************************************72
c
cc TEST01 compares WAPR to stored values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 June 2014
c
c  Author:
c
c    Original FORTRAN77 version by Andrew Barry, S. J. Barry, 
c    Patricia Culligan-Hensley.
c    This FORTRAN77 version by John Burkardt.
c
c  Parameters:
c
c    Input, integer NBITS, the number of bits in the mantissa.
c
      implicit none

      double precision dx1(68)
      double precision em
      integer i
      integer nbits
      integer nd
      integer nerror
      double precision w
      double precision wapr
      double precision wm1(68)
      double precision wm2(68)
      double precision wp1(68)
      double precision wp2(40)
      double precision wp3(10)
      double precision x2(20)
      double precision x3(10)

      save
c
c  Exact results for Wp(x).
c
c  X close to -exp(-1).
c
      data dx1 /
     &     1.D-40,2.D-40,3.D-40,4.D-40,5.D-40,6.D-40,7.D-40,8.D-40,
     &     9.D-40,1.D-39,1.D-30,2.D-30,3.D-30,4.D-30,5.D-30,6.D-30,
     &     7.D-30,8.D-30,9.D-30,1.D-29,1.D-20,2.D-20,3.D-20,4.D-20,
     &     5.D-20,6.D-20,7.D-20,8.D-20,9.D-20,1.D-19,1.D-10,2.D-10,
     &     3.D-10,4.D-10,5.D-10,6.D-10,7.D-10,8.D-10,9.D-10,1.D-9,1.D-5,
     &     2.D-5,3.D-5,4.D-5,5.D-5,6.D-5,7.D-5,8.D-5,9.D-5,1.D-4,2.D-4,
     &     3.D-4,4.D-4,5.D-4,6.D-4,7.D-4,8.D-4,9.D-4,1.D-3,2.D-3,3.D-3,
     &     4.D-3,5.D-3,6.D-3,7.D-3,8.D-3,9.D-3,1.D-2/
      data (wp1(i),i=1,10)/
     &     -.9999999999999999999766835601840287579665D0,
     &     -.9999999999999999999670255745859974370634D0,
     &     -.9999999999999999999596147415871158855702D0,
     &     -.9999999999999999999533671203680575159335D0,
     &     -.9999999999999999999478628555782056161596D0,
     &     -.9999999999999999999428866198325571498809D0,
     &     -.9999999999999999999383104987875354650364D0,
     &     -.9999999999999999999340511491719948741275D0,
     &     -.9999999999999999999300506805520862739007D0,
     &     -.9999999999999999999262669432552936235556D0/
      data (wp1(i),i=11,20)/
     &     -.9999999999999976683560184028776088243496D0,
     &     -.9999999999999967025574585997473306784697D0,
     &     -.9999999999999959614741587115939935280812D0,
     &     -.9999999999999953367120368057588420244704D0,
     &     -.9999999999999947862855578205706768098859D0,
     &     -.9999999999999942886619832557258611080314D0,
     &     -.9999999999999938310498787535591888253966D0,
     &     -.999999999999993405114917199501910108482D0,
     &     -.9999999999999930050680552086436996003626D0,
     &     -.9999999999999926266943255293804772564852D0/
      data (wp1(i),i=21,30)/
     &     -.9999999997668356018584094585181033975713D0,
     &     -.9999999996702557458962181283375794922681D0,
     &     -.9999999995961474159255244922555603070484D0,
     &     -.9999999995336712037530626747373742782638D0,
     &     -.9999999994786285558726655558473617503914D0,
     &     -.9999999994288661984343027719079710168757D0,
     &     -.9999999993831049880022078023099082447845D0,
     &     -.9999999993405114918649237720678678043098D0,
     &     -.9999999993005068056839596486461928553989D0,
     &     -.9999999992626694327341550240404575805522D0/
      data (wp1(i),i=31,40)/
     &     -.9999766837414008807143234266407434345965D0,
     &     -.9999670259370180970391011806287847685011D0,
     &     -.9999596152852334187587360603177913882165D0,
     &     -.9999533678452277190993205651165793258207D0,
     &     -.9999478637616504968301143759542621752943D0,
     &     -.9999428877071168268324462680980110604599D0,
     &     -.999938311767283189655618359697592754013D0,
     &     -.999934052598878483932035240348113191731D0,
     &     -.9999300523114688962155368933174163936848D0,
     &     -.9999262687553819399632780281900349826094D0/
      data (wp1(i),i=41,50)/
     &     -.9926447551971221136721993073029112268763D0,
     &     -.9896086425917686478635208903220735023288D0,
     &     -.9872831094708759013315476674998231771112D0,
     &     -.9853253899681719161468126266199947992874D0,
     &     -.9836027178149637071691226667555243369797D0,
     &     -.9820470029764667038452666345865058694192D0,
     &     -.9806177971936827573257045283891513709368D0,
     &     -.97928874641099293421931043027104578327D0,
     &     -.9780415451927629881943028498821429186059D0,
     &     -.9768628655744219140604871252425961901255D0/
      data (wp1(i),i=51,59)/
     &     -.967382626983074241885253344632666448927D0,
     &     -.9601485420712594199373375259293860324633D0,
     &     -.9540768694875733222057908617314370634111D0,
     &     -.9487478690765183543410579996573536771348D0,
     &     -.9439462911219380338176477772712853402016D0,
     &     -.9395442782590063946376623684916441100361D0,
     &     -.9354585313336439336066341889767099608018D0,
     &     -.9316311953818583253420613278986500351794D0,
     &     -.9280201500545670487600430252549212247489D0/
      data (wp1(i),i=60,68)/
     &     -.8991857663963733198571950343133631348347D0,
     &     -.8774287170665477623395641312875506084256D0,
     &     -.8593275036837387237312746018678000939451D0,
     &     -.8435580020488052057849697812109882706542D0,
     &     -.8294416857114015557682843481727604063558D0,
     &     -.8165758053803078481644781849709953302847D0,
     &     -.8046981564792468915744561969751509934994D0,
     &     -.7936267540949175059651534957734689407879D0,
     &     -.7832291989812967764330746819464532478021D0/
c
c  x close to 0.
c
      data x2 /
     &     1.D-9,2.D-9,3.D-9,4.D-9,5.D-9,6.D-9,7.D-9,8.D-9,9.D-9,1.D-8,
     &     1.D-2,2.D-2,3.D-2,4.D-2,5.D-2,6.D-2,7.D-2,8.D-2,9.D-2,1.D-1/
      data (wp2(i),i=1,10)/
     &     9.999999990000000014999999973333333385417D-10,
     &     1.9999999960000000119999999573333335D-9,
     &     2.999999991000000040499999784000001265625D-9,
     &     3.999999984000000095999999317333338666667D-9,
     &     4.999999975000000187499998333333349609375D-9,
     &     5.999999964000000323999996544000040499999D-9,
     &     6.99999995100000051449999359733342086979D-9,
     &     7.999999936000000767999989077333503999997D-9,
     &     8.999999919000001093499982504000307546869D-9,
     &     9.999999900000001499999973333333854166656D-9/
      data (wp2(i),i=11,20)/
     &     9.901473843595011885336326816570107953628D-3,
     &     1.961158933740562729168248268298370977812D-2,
     &     2.913845916787001265458568152535395243296D-2,
     &     3.848966594197856933287598180923987047561D-2,
     &     4.767230860012937472638890051416087074706D-2,
     &     5.669304377414432493107872588796066666126D-2,
     &     6.555812274442272075701853672870305774479D-2,
     &     7.427342455278083997072135190143718509109D-2,
     &     8.284448574644162210327285639805993759142D-2,
     &     9.127652716086226429989572142317956865312D-2/
      data (wp2(i),i=21,30)/
     &     -1.000000001000000001500000002666666671875D-9,
     &     -2.000000004000000012000000042666666833333D-9,
     &     -3.000000009000000040500000216000001265625D-9,
     &     -4.000000016000000096000000682666672D-9,
     &     -5.000000025000000187500001666666682942709D-9,
     &     -6.000000036000000324000003456000040500001D-9,
     &     -7.000000049000000514500006402666754203126D-9,
     &     -8.000000064000000768000010922666837333336D-9,
     &     -9.000000081000001093500017496000307546881D-9,
     &     -1.000000010000000150000002666666718750001D-8/
      data (wp2(i),i=31,40)/
     &     -1.010152719853875327292018767138623973671D-2,
     &     -2.041244405580766725973605390749548004159D-2,
     &     -3.094279498284817939791038065611524917276D-2,
     &     -4.170340843648447389872733812553976786256D-2,
     &     -5.270598355154634795995650617915721289428D-2,
     &     -6.396318935617251019529498180168867456393D-2,
     &     -7.548877886579220591933915955796681153525D-2,
     &     -8.729772086157992404091975866027313992649D-2,
     &     -9.940635280454481474353567186786621057821D-2,
     &     -1.118325591589629648335694568202658422726D-1/
c
c  Other Wp results.
c
      data x3/1.D1,1.D2,1.D3,1.D4,1.D5,1.D6,1.D7,1.D8,1.D9,1.D10/
      data wp3/
     &     1.745528002740699383074301264875389911535D0,
     &     3.385630140290050184888244364529726867492D0,
     &     5.249602852401596227126056319697306282521D0,
     &     7.231846038093372706475618500141253883968D0,
     &     9.284571428622108983205132234759581939317D0,
     &     11.38335808614005262200015678158500428903D0,
     &     13.5143440103060912090067238511621580283D0,
     &     15.66899671545096218719628189389457073619D0,
     &     17.84172596742146918254060066535711011039D0,
     &     20.02868541330495078123430607181488729749D0/
c
c  Exact results for Wm(x).
c
c  X close to -exp(-1).
c
      data (wm1(i),i=1,10)/
     &     -1.000000000000000000023316439815971242034D0,
     &     -1.000000000000000000032974425414002562937D0,
     &     -1.000000000000000000040385258412884114431D0,
     &     -1.000000000000000000046632879631942484068D0,
     &     -1.000000000000000000052137144421794383842D0,
     &     -1.000000000000000000057113380167442850121D0,
     &     -1.000000000000000000061689501212464534966D0,
     &     -1.000000000000000000065948850828005125875D0,
     &     -1.000000000000000000069949319447913726103D0,
     &     -1.000000000000000000073733056744706376448D0/
      data (wm1(i),i=11,20)/
     &     -1.000000000000002331643981597126015551422D0,
     &     -1.000000000000003297442541400259918073073D0,
     &     -1.000000000000004038525841288416879599233D0,
     &     -1.000000000000004663287963194255655478615D0,
     &     -1.00000000000000521371444217944744506897D0,
     &     -1.000000000000005711338016744295885146596D0,
     &     -1.000000000000006168950121246466181805002D0,
     &     -1.000000000000006594885082800527084897688D0,
     &     -1.000000000000006994931944791388919781579D0,
     &     -1.000000000000007373305674470655766501228D0/
      data (wm1(i),i=21,30)/
     &     -1.000000000233164398177834299194683872234D0,
     &     -1.000000000329744254176269387087995047343D0,
     &     -1.00000000040385258418320678088280150237D0,
     &     -1.000000000466328796391912356113774800963D0,
     &     -1.000000000521371444308553232716574598644D0,
     &     -1.00000000057113380178315977436875260197D0,
     &     -1.000000000616895012251498501679602643872D0,
     &     -1.000000000659488508425026289634430354159D0,
     &     -1.000000000699493194642234170768892572882D0,
     &     -1.000000000737330567628282553087415117543D0/
      data (wm1(i),i=31,40)/
     &     -1.000023316621036696460620295453277856456D0,
     &     -1.000032974787857057404928311684626421503D0,
     &     -1.000040385802079313048521250390482335902D0,
     &     -1.00004663360452259016530661221213359488D0,
     &     -1.0000521380505373899860247162705706318D0,
     &     -1.000057114467508637629346787348726350223D0,
     &     -1.000061690769779852545970707346938004879D0,
     &     -1.000065950300622136103491886720203687457D0,
     &     -1.00006995095046930174807034225078340539D0,
     &     -1.000073734868993836022551364404248563489D0/
      data (wm1(i),i=41,50)/
     &     -1.007391489031309264813153180819941418531D0,
     &     -1.010463846806564696239430620915659099361D0,
     &     -1.012825626038880105597738761474228363542D0,
     &     -1.014819592594577564927398399257428492725D0,
     &     -1.016578512742400177512255407989807698099D0,
     &     -1.018170476517182636083407324035097024753D0,
     &     -1.019635932177973215702948823070039007361D0,
     &     -1.021001233780440980009663527189756124983D0,
     &     -1.022284686760270309618528459224732558767D0,
     &     -1.023499619082082348038906498637836105447D0/
      data (wm1(i),i=51,59)/
     &     -1.033342436522109918536891072083243897233D0,
     &     -1.040939194524944844012076988438386306843D0,
     &     -1.047373634492196231421755878017895964061D0,
     &     -1.053065496629607111615572634090884988897D0,
     &     -1.058230030703619902820337106917657189605D0,
     &     -1.06299509412938704964298950857825124135D0,
     &     -1.067443986111355120560366087010834293872D0,
     &     -1.071634561663924136735470389832541413862D0,
     &     -1.07560894118662498941494486924522316597D0/
      data (wm1(i),i=60,68)/
     &     -1.108081880631165502564629660944418191031D0,
     &     -1.133487001006868638317076487349933855181D0,
     &     -1.155245851821528613609784258821055266176D0,
     &     -1.174682608817289477552149783867901714817D0,
     &     -1.192475850408615960644596781702366026321D0,
     &     -1.209028378276581220769059281765172085749D0,
     &     -1.224602449817731587352403997390766826335D0,
     &     -1.239380103200799714836392811991400433357D0,
     &     -1.253493791367214516100457405907304877145D0/
c
c  X close to 0
c
      data (wm2(i),i=1,10)/
     &     -96.67475603368003636615083422832414231073D0,
     &     -95.97433737593292677679699834708774152264D0,
     &     -95.56459382507349364043974513871359837914D0,
     &     -95.27386489130628866261760496192716897551D0,
     &     -95.0483515329550645558378163981346085731D0,
     &     -94.86408948075132603599669916724611501067D0,
     &     -94.70829516116125928735505687861553800851D0,
     &     -94.57333777268864473984718625104978190798D0,
     &     -94.45429521137271454134108055166168787618D0,
     &     -94.34780665137385269060032461028588648619D0/
      data (wm2(i),i=11,20)/
     &     -73.37311031382297679706747875812087452918D0,
     &     -72.67033891766978907253811160121558649554D0,
     &     -72.25920015786413889986246462168541066725D0,
     &     -71.9674726772681844325410195162521230103D0,
     &     -71.74117979478106456261839111929684980709D0,
     &     -71.55627755942675731851469544735603279342D0,
     &     -71.39993966508440988906136771384270319452D0,
     &     -71.26450969134836299738230265916604879247D0,
     &     -71.14504894849287026061378869987876478469D0,
     &     -71.03818524971357411174259539994036186653D0/
      data (wm2(i),i=21,30)/
     &     -49.96298427667447244531514297262540669957D0,
     &     -49.25557728489066973476436802404294348267D0,
     &     -48.84167348449764278180827692232244878061D0,
     &     -48.54795966722336777861228992424053274064D0,
     &     -48.32011181512544381639923088992262632623D0,
     &     -48.13392971864323755677656581326964166718D0,
     &     -47.97650308277095858785998266172487372203D0,
     &     -47.84012504158555569017852884133421231303D0,
     &     -47.71982419568730714141619502661365943996D0,
     &     -47.61220592218922310708330388890925734186D0/
      data (wm2(i),i=31,40)/
     &     -26.29523881924692569411012882185491823773D0,
     &     -25.57429135222126159950976461862116397347D0,
     &     -25.15218334705420805339928870686463192335D0,
     &     -24.85251554543232259250342343156454440592D0,
     &     -24.61997095867949438248843689371454503831D0,
     &     -24.42989922074834124324823589226341161866D0,
     &     -24.26914663885402405126372567664280388966D0,
     &     -24.12985944288624210229238972590881794092D0,
     &     -24.00697058168597098928369714836882130512D0,
     &     -23.8970195845316574350263109196222825525D0/
      data (wm2(i),i=41,50)/
     &     -14.16360081581018300910955630361089957762D0,
     &     -13.41624453595298662833544556875899262976D0,
     &     -12.97753279184081358418630625949303360266D0,
     &     -12.66551396826200331850774017793451747947D0,
     &     -12.42304039760186078066171146072124458063D0,
     &     -12.22461776385387453853455424320739669321D0,
     &     -12.05663003490708840623665404674007291018D0,
     &     -11.91094134143842011964821167497982287763D0,
     &     -11.78229922740701885487699061601349928173D0,
     &     -11.66711453256635441837882744697047370583D0/
      data (wm2(i),i=51,59)/
     &     -10.90655739570090676132157335673785028979D0,
     &     -10.45921112040100393534625826514848865968D0,
     &     -10.14059243262036578763968437893562720385D0,
     &     -9.892699522704254067620287857665824159861D0,
     &     -9.689637966382397752838283301312347921626D0,
     &     -9.517569762038614935107630230444563521109D0,
     &     -9.368222172408836799233763466046500781388D0,
     &     -9.236251966692597369166416348621131600216D0,
     &     -9.11800647040274012125833718204681427427D0/
      data (wm2(i),i=60,68)/
     &     -8.335081377982507150789361715143483020265D0,
     &     -7.872521380098708883395239767904984410792D0,
     &     -7.541940416432904084217222998374802941589D0,
     &     -7.283997135099081646930521042317118095276D0,
     &     -7.072162048994701667487346245044653243434D0,
     &     -6.892241486671583156187212318718730022068D0,
     &     -6.735741661607793269808533725369490789074D0,
     &     -6.597171733627119347342347717832724288261D0,
     &     -6.472775124394004694741057892724488037104D0/
c
c  Compare the approximations of WAPR with the given exact values.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  Compare WAPR(X) to stored values.'
c
c  Wp results for x near -exp(-1).
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Wp results for x near -exp(-1)'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '       Offset x        W(x) (WAPR)' //
     &  '     W(x) (EXACT)   Digits Correct'
      write ( *, '(a)' ) ''

      do i = 1, 68

        if ( dx1(i) .eq. 0.0D+00 ) then
          em = - dexp ( -1.0D+00 )
          w = wapr ( em, 0, nerror, 0 )
        else
          w = wapr ( dx1(i), 0, nerror, 1 )
        end if

        if ( w .eq. wp1(i) ) then
          nd = int ( dlog10 ( 2.0D+00 ** nbits ) + 0.5D+00 )
        else
          nd = int ( dlog10 ( dabs ( wp1(i) / ( w - wp1(i) ))) 
     &      + 0.5D+00 )
        end if

        write ( *, '(3(1P1D17.8),6X,I3)' ) dx1(i), w, wp1(i), nd

      end do
c
c  Wp results for x near 0.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Wp results for x near 0'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          x            W(x) (WAPR)' // 
     &  '     W(x) (EXACT)   Digits Correct'
      write ( *, '(a)' ) ''

      do i = 1, 20
        w = wapr ( x2(i), 0, nerror, 0 )
        if ( w .eq. wp2(i) ) then
          nd = int ( dlog10 ( 2.0D+00 ** nbits ) + 0.5D+00 )
        else
          nd = int ( dlog10 ( dabs ( wp2(i) / ( w - wp2(i) ))) 
     &      + 0.5D+00 )
        end if
        write ( *, '(3(1P1D17.8),6X,I3)' ) x2(i), w, wp2(i), nd
      end do

      do i = 1, 20
        w = wapr ( -x2(i), 0, nerror, 0 )
        if ( w .eq. wp2(20+i) ) then
          nd = int ( dlog10 ( 2.0D+00 ** nbits ) + 0.5D+00 )
        else
          nd = int ( dlog10 ( dabs ( wp2(20+i) / ( w - wp2(20+i) ) ) )
     &      + 0.5D+00 )
        end if
        write ( *, '(3(1P1D17.8),6X,I3)' ) -x2(i), w, wp2(20+i), nd
      end do
c
c  Other Wp results.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Other Wp results'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '          x             W(x) (WAPR)' //
     &  '     W(x) (EXACT)   Digits Correct'
      write ( *, '(a)' ) ''

      do i = 1, 10
        w = wapr ( x3(i), 0, nerror, 0 )
        if ( w .eq. wp3(i) ) then
          nd = int ( dlog10 ( 2.0D+00 ** nbits ) + 0.5D+00 )
        else
          nd = int ( dlog10 ( dabs ( wp3(i) / ( w - wp3(i) ))) 
     &      + 0.5D+00 )
        end if
        write ( *, '(3(1P1D17.8),6X,I3)' ) x3(i), w, wp3(i), nd
      end do
c
c  Wm results for x near -exp(-1).
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Wm results for x near 0'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '          x            W(x) (WAPR)' //
     &  '     W(x) (EXACT)   Digits Correct'
      write ( *, '(a)' ) ''

      do i = 1, 68
        w = wapr ( dx1(i), 1, nerror, 1 )
        if ( w .eq. wm1(i) ) then
          nd = int ( dlog10 ( 2.0D+00 ** nbits ) + 0.5D+00 )
        else
          nd = int ( dlog10 ( dabs ( wm1(i) / ( w - wm1(i) ))) 
     &      + 0.5D+00 )
        end if
        write ( *, '(3(1P1D17.8),6X,I3)' ) dx1(i), w, wm1(i), nd
      end do
c
c  Wm results for x near 0.
c  Check for underflow.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Wm results for x near -exp(-1)'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '       Offset x        W(x) (WAPR)' //
     &  '     W(x) (EXACT)   Digits Correct'
      write ( *, '(a)' ) ''

      do i = 1, 68

        if ( 0.0D+00 .lt. dx1(i) ) then
          w = wapr ( -dx1(i), 1, nerror, 0 )
          if ( w .eq. wm2(i) ) then
            nd = int ( dlog10 ( 2.0D+00 ** nbits ) + 0.5D+00 )
          else
            nd = int ( dlog10 ( dabs ( wm2(i) / ( w - wm2(i) ))) 
     &        + 0.5D+00 )
          end if
          write ( *, '(3(1P1D17.8),6X,I3)' ) -dx1(i), w, wm2(i), nd
        end if

      end do

      return
      end
      subroutine test02 ( nbits, dx, n )

c*********************************************************************72
c
cc TEST02 tests WAPR(X) when X is the offset of the argument from -exp(-1).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 June 2014
c
c  Author:
c
c    Original FORTRAN77 version by Andrew Barry, S. J. Barry, 
c    Patricia Culligan-Hensley.
c    This FORTRAN77 version by John Burkardt.
c
c  Parameters:
c
c    Input, integer NBITS, the number of bits in the mantissa.
c
c    Input, double precision DX, the initial offset.
c
c    Input, integer N, the number of offset arguments to generate.
c
      implicit none

      double precision bisect
      double precision dx
      integer i
      integer ifmt
      integer iw
      integer l
      integer n
      integer nbits
      integer nd
      integer ner
      integer nerror
      double precision w
      double precision wapr
      double precision we
      double precision x
      double precision xmax
      double precision xmin

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) '  Input X is the offset from -exp(-1).'

      l = 1
      ifmt = 0
      xmax = n * dx - dexp ( -1.0D+00 )
      xmin = 0.0D+00

      if ( xmax .le. 0.0D+00 ) then
        iw = 1
        write ( *, '(a)' )
     &    '  Both branches of the W function will be checked.'
      else
        iw = 0
        write ( *, '(a)' ) '  Wp has been selected (maximum x is > 0)'
      end if

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Results for Wp(x):'

      if ( ifmt .eq. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) '       Offset x        W(x) (WAPR)' //
     &    '     W(x) (BISECT)  Digits Correct'
      else
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) '         x             W(x) (WAPR)' //
     &      '     W(x) (BISECT)  Digits Correct'
      end if

      write ( *, '(a)' ) ''

      do i = 1, n + 1

        x = xmin + i * dx
        w = wapr ( x, 0, nerror, l )

        if ( nerror .eq. 1 ) then

          write ( *, '(a,d16.8,a)' ) 
     &      '  The value of X = ', x, ' is out of range.'

        else

          we = bisect ( x, 0, ner, l )

          if ( ner .eq. 1 ) then
            write ( *, '(a)' ) ''
            write ( *, '(a,g14.6)' ) 
     &        ' BISECT did not converge for x =', x
            write ( *, '(a)' ) '  Try reducing NBITS.'
          end if

          if ( w .eq. we ) then
            nd = int ( dlog10 ( 2.0D+00 ** nbits ) + 0.5D+00 )
          else
            nd = int ( dlog10 ( dabs ( we / ( w - we ) ) ) + 0.5D+00 )
          end if

          write ( *, '(3(1P1D17.8),6X,I3)' ) x, w, we, nd

        end if

      end do

      if ( iw .eq. 1 ) then

        write ( *, '(a)' ) ''
        write ( *, '(a)' ) '  Results for Wm(x):'

        if ( ifmt .eq. 0 ) then
          write ( *, '(a)' ) ''
          write ( *, '(a)' ) '       Offset x        W(x) (WAPR)' //
     &      '     W(x) (BISECT)  Digits Correct'
        else
          write ( *, '(a)' ) ''
          write ( *, '(a)' ) '         x             W(x) (WAPR)' //
     &      '     W(x) (BISECT)  Digits Correct'
        end if

        do i = 1, n + 1

          x = xmin + i * dx
          w = wapr ( x, 1, nerror, l )

          if ( nerror .eq. 1 ) then

            write ( *, '(a,d16.8,a)' ) 
     &        '  The value of X = ', x, ' is out of range.'

          else

            we = bisect ( x, 1, ner, l )

            if ( ner .eq. 1 ) then
              write ( *, '(a)' ) ''
              write ( *, '(a,g14.6)' ) 
     &          ' BISECT did not converge for x =', x
              write ( *, '(a)' ) '  Try reducing NBITS.'
            end if
  
            if ( w .eq. we ) then
              nd = int ( dlog10 ( 2.0D+00 ** nbits ) + 0.5D+00 )
            else
              nd = int ( dlog10 ( dabs ( we / ( w - we ) ) ) + 0.5D+00 )
            end if

            write ( *, '(3(1P1D17.8),6X,I3)' ) x, w, we, nd

          end if

        end do

      end if

      return
      end
      subroutine test03 ( nbits, xmin, xmax, n )

c*********************************************************************72
c
cc TEST03 tests WAPR(X) when X is the argument.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 June 2014
c
c  Author:
c
c    Original FORTRAN77 version by Andrew Barry, S. J. Barry, 
c    Patricia Culligan-Hensley.
c    This FORTRAN77 version by John Burkardt.
c
c  Parameters:
c
c    Input, integer NBITS, the number of bits in the mantissa.
c
c    Input, double precision XMIN, XMAX, the range.
c
c    Input, integer N, the number of equally spaced values
c    in the range at which arguments are to be chosen.
c
      implicit none

      double precision bisect
      double precision dx
      integer i
      integer ifmt
      integer iw
      integer l
      integer n
      integer nbits
      integer nd
      integer ner
      integer nerror
      double precision temp
      double precision w
      double precision wapr
      double precision we
      double precision x
      double precision xmax
      double precision xmin

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) '  Input X is the argument.'

      l = 0
      ifmt = 1

      if ( xmax .lt. xmin ) then
        temp = xmin
        xmin = xmax
        xmax = temp
      end if

      dx = ( xmax - xmin ) / dble ( n )
      xmin = xmin - dx

      if ( xmax .le. 0.0D+00 ) then
        iw = 1
        write ( *, '(a)' ) 
     &    '  Both branches of the W function will be checked.'
      else
        iw = 0
        write ( *, '(a)' ) '  Wp has been selected (maximum x is > 0)'
      end if

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Results for Wp(x):'

      if ( ifmt .eq. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) '       Offset x        W(x) (WAPR)' //
     &      '     W(x) (BISECT)  Digits Correct'
      else
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) '         x             W(x) (WAPR)' //
     &      '     W(x) (BISECT)  Digits Correct'
      end if
      write ( *, '(a)' ) ''

      do i = 1, n + 1

        x = xmin + i * dx
        w = wapr ( x, 0, nerror, l )

        if ( nerror .eq. 1 ) then

          write ( *, '(a,d16.8,a)' ) 
     &      '  The value of X = ', x, ' is out of range.'

        else

          we = bisect ( x, 0, ner, l )

          if ( ner .eq. 1 ) then
            write ( *, '(a)' ) ''
            write ( *, '(a,g14.6)' ) 
     &        ' BISECT did not converge for x =', x
            write ( *, '(a)' ) '  Try reducing NBITS.'
          end if

          if ( w .eq. we ) then
            nd = int ( dlog10 ( 2.0D+00 ** nbits ) + 0.5D+00 )
          else
            nd = int ( dlog10 ( dabs ( we / ( w - we ))) + 0.5D+00 )
          end if

          write ( *, '(3(1P1D17.8),6X,I3)' ) x, w, we, nd

        end if

      end do

      if ( iw .eq. 1 ) then

        write ( *, '(a)' ) ''
        write ( *, '(a)' ) '  Results for Wm(x):'

        if ( ifmt .eq. 0 ) then
          write ( *, '(a)' ) ''
          write ( *, '(a)' ) '       Offset x        W(x) (WAPR)' //
     &      '     W(x) (BISECT)  Digits Correct'
        else
          write ( *, '(a)' ) ''
          write ( *, '(a)' ) '         x             W(x) (WAPR)' //
     &      '     W(x) (BISECT)  Digits Correct'
        end if

        do i = 1, n + 1

          x = xmin + i * dx
          w = wapr ( x, 1, nerror, l )

          if ( nerror .eq. 1 ) then

            write ( *, '(a,d16.8,a)' ) 
     &        '  The value of X = ', x, ' is out of range.'

          else

            we = bisect ( x, 1, ner, l )

            if ( ner .eq. 1 ) then
              write ( *, '(a)' ) ''
              write ( *, '(a,g14.6)' ) 
     &          ' BISECT did not converge for x =', x
              write ( *, '(a)' ) '  Try reducing NBITS.'
            end if

            if ( w .eq. we ) then
              nd = int ( dlog10 ( 2.0D+00 ** nbits ) + 0.5D+00 )
            else
              nd = int ( dlog10 ( dabs ( we / ( w - we ))) + 0.5D+00 )
            end if

            write ( *, '(3(1P1D17.8),6X,I3)' ) x, w, we, nd

          end if

        end do

      end if

      return
      end
