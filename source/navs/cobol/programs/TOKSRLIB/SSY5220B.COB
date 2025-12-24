****************************************************************
*    å⁄ãqñºÅ@Å@Å@Å@Å@Å@Å@ÅFÅ@ÅiäîÅjÉTÉJÉ^ÇÃÉ^ÉlìaÅ@Å@Å@Å@Å@Å@Å@*
*    ã∆ñ±ñºÅ@Å@Å@Å@Å@Å@Å@ÅFÅ@éÛíçÅ@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@        *
*    ÉTÉuÉVÉXÉeÉÄÅ@Å@Å@Å@ÅFÅ@ó¨í ÇaÇlÇrÅ@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@*
*    ÉÇÉWÉÖÅ[ÉãñºÅ@Å@Å@Å@ÅFÅ@î[ïiì˙àÍäáïœçXÅ@Å@Å@Å@Å@Å@Å@Å@Å@Å@*
*    çÏê¨ì˙Å^çÏê¨é“Å@Å@Å@ÅFÅ@2019/07/24 NAV INOUE              *
*    èàóùäTóvÅ@Å@Å@Å@Å@Å@ÅFÅ@îÑè„ì`ï[ÉtÉ@ÉCÉãÅEÇaÇlÇrî≠íçÇlÇrÇf*
*                          Å@ÇÃî[ïiì˙Åiç≈èIî[ïiêÊî[ïiì˙ÅjÇÅ@Å@*
*                          Å@àÍäáïœçXÇ∑ÇÈÅBÅ@Å@Å@Å@Å@Å@Å@Å@Å@Å@*
*<óöó>*********************************************************
* XXXX/XX/XX XXXXXXXXX ÇmÇmÇmÇmÇmÇmÇmÇmÇmÇmÇmÇmÇmÇmÇmÇmÇmÇmÇmÇm
* 2019/07/24 INOUE Å@Å@êVãKçÏê¨  ó¨ópÅFSSY3883B.TOKSRLIB
*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY5220B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2019/07/24.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< ì`ï[ÉfÅ[É^ >>--*
     SELECT   SHTDENLJ  ASSIGN         DA-01-VI-SHTDENLJ
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  DEN-F46   DEN-F47
                                       DEN-F01   DEN-F112
                                       DEN-F48   DEN-F02
                                       DEN-F04   DEN-F051
                                       DEN-F07   DEN-F03
                        WITH  DUPLICATES
                        FILE STATUS    DEN-ST.
*---<<  ÇaÇlÇrî≠íçÇlÇrÇfÅ@Å@Å@  >>---*
     SELECT   BMSHACL3  ASSIGN    TO   DA-01-VI-BMSHACL3
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  HAC-F011 HAC-F012
                                       HAC-F013 HAC-F02
                                       HAC-F346 HAC-F308
                                       HAC-F302 HAC-F402
                        WITH  DUPLICATES
                        FILE STATUS    HAC-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< ì`ï[ÉfÅ[É^ >>--*
 FD  SHTDENLJ           LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN       PREFIX.
*----<< ÇaÇlÇrî≠íçÇlÇrÇf >>--*
 FD  BMSHACL3           LABEL RECORD   IS   STANDARD.
     COPY     BMSHACL3  OF        XFDLIB
              JOINING   HAC       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  SHTDENLJ-INV-FLG    PIC  X(03)  VALUE  SPACE.
     03  SHTDENLJ-END-FLG    PIC  X(03)  VALUE  SPACE.
     03  BMSHACL3-INV-FLG    PIC  X(03)  VALUE  SPACE.
     03  BMSHACL3-END-FLG    PIC  X(03)  VALUE  SPACE.
*
*----<< Ã±≤Ÿ Ω√∞¿Ω >>--*
 01  DEN-ST            PIC  X(02)        VALUE  SPACE.
 01  HAC-ST            PIC  X(02)        VALUE  SPACE.
*
*----<< CNT        >>--*
 01  DEN-CNT           PIC  9(07)        VALUE  ZERO.
 01  HAC-CNT           PIC  9(07)        VALUE  ZERO.
*----<< WORK       >>--*
 01  WK-DEN.
     03 WK-DENNO       PIC  9(09)        OCCURS 20.
 01  IX                PIC  9(02)        VALUE  ZERO.
*----<< À¬ﬁπ ‹∞∏ >>--*
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-DATEW          PIC  9(08).
 01  FILLER             REDEFINES      SYS-DATEW.
     03  SYS-YYW        PIC  9(04).
     03  SYS-MMW        PIC  9(02).
     03  SYS-DDW        PIC  9(02).
 01  WK-SYSYMD.
     03  WK-SYSYY       PIC  9(04).
     03  FILLER         PIC  X(01)     VALUE "/".
     03  WK-SYSMM       PIC  Z9.
     03  FILLER         PIC  X(01)     VALUE "/".
     03  WK-SYSDD       PIC  Z9.
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
*01  SYS-TIME2          PIC  9(08).
*01  FILLER             REDEFINES      SYS-TIME2.
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-TIMEW      PIC  9(06).
     03  FILLER         PIC  9(02).
*
 01  WRK-NAME.
     03  WRK-KANA            PIC  X(15)        VALUE SPACE.
*
*åvéZóÃàÊ
 01  WRK-AREA2.
     03  WRK-HIK             PIC S9(09)V9(02)  VALUE ZERO.
     03  WRK-ZAI             PIC S9(09)V9(02)  VALUE ZERO.
*
 01  SEC-AREA.
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                    SECTION.
 01  LINK-IN-BUMON          PIC X(04).
 01  LINK-IN-TANCD          PIC X(02).
 01  LINK-IN-DATE           PIC 9(08).
 01  LINK-IN-TIME           PIC 9(04).
 01  LINK-IN-TORICD         PIC 9(08).
 01  LINK-IN-SOKCD          PIC X(02).
 01  LINK-IN-FDATE          PIC 9(08).
 01  LINK-IN-TDATE          PIC 9(08).
 01  LINK-IN-DENNO.
     03  LINK-IN-DENNO-01   PIC  X(02).
     03  LINK-IN-DENNO-02   PIC  X(02).
     03  LINK-IN-DENNO-03   PIC  X(02).
     03  LINK-IN-DENNO-04   PIC  X(02).
     03  LINK-IN-DENNO-05   PIC  X(02).
     03  LINK-IN-DENNO-06   PIC  X(02).
     03  LINK-IN-DENNO-07   PIC  X(02).
     03  LINK-IN-DENNO-08   PIC  X(02).
     03  LINK-IN-DENNO-09   PIC  X(02).
     03  LINK-IN-DENNO-10   PIC  X(02).
     03  LINK-IN-DENNO-11   PIC  X(02).
     03  LINK-IN-DENNO-12   PIC  X(02).
     03  LINK-IN-DENNO-13   PIC  X(02).
     03  LINK-IN-DENNO-14   PIC  X(02).
     03  LINK-IN-DENNO-15   PIC  X(02).
     03  LINK-IN-DENNO-16   PIC  X(02).
     03  LINK-IN-DENNO-17   PIC  X(02).
     03  LINK-IN-DENNO-18   PIC  X(02).
     03  LINK-IN-DENNO-19   PIC  X(02).
     03  LINK-IN-DENNO-20   PIC  X(02).
*
****************************************************************
 PROCEDURE              DIVISION  USING
                                  LINK-IN-BUMON
                                  LINK-IN-TANCD
                                  LINK-IN-DATE
                                  LINK-IN-TIME
                                  LINK-IN-TORICD
                                  LINK-IN-SOKCD
                                  LINK-IN-FDATE
                                  LINK-IN-TDATE
                                  LINK-IN-DENNO.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        ÉGÉâÅ[èàóùÅ@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@ *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< ì`ï[ÉfÅ[É^ >>--*
 SHTDENLJ-ERR           SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHTDENLJ.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY5220B SHTDENLJ ERROR " DEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     STOP     RUN.
*----<< ÇaÇlÇrî≠íçÇlÇrÇfÅ@Å@Å@ >>--*
 BMSHACL3-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      BMSHACL3.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY5220B BMSHACL3 ERROR " HAC-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     Ãﬂ€∏ﬁ◊— ∫›ƒ€∞Ÿ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     MOVE    "000-PROG-CNTL"      TO   S-NAME.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN.
     PERFORM  300-END-RTN.
 000-PROG-CNTL-EXIT.
     EXIT     PROGRAM.
*--------------------------------------------------------------*
*    LEVEL  2      ºÆ∑ ºÆÿ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
     MOVE    "100-INIT-RTN"       TO   S-NAME.
*
 100-INIT-01.
     ACCEPT   SYS-DATE       FROM DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYS-DATE  TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
         MOVE LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE ZERO           TO   SYS-DATEW
     END-IF.
*
     ACCEPT   SYS-TIME       FROM TIME.
*
*TÅ´
*****DISPLAY "LINK-IN-BUMON = " LINK-IN-BUMON  UPON CONS.
*****DISPLAY "LINK-IN-TANCD = " LINK-IN-TANCD  UPON CONS.
*****DISPLAY "LINK-IN-DATE  = " LINK-IN-DATE   UPON CONS.
*****DISPLAY "LINK-TIME     = " LINK-TIME      UPON CONS.
*****DISPLAY "LINK-TORICD   = " LINK-TORICD    UPON CONS.
*****DISPLAY "LINK-SOKCD    = " LINK-SOKCD     UPON CONS.
*****DISPLAY "LINK-FDATE    = " LINK-FDATE     UPON CONS.
*****DISPLAY "LINK-TDATE    = " LINK-TDATE     UPON CONS.
*****DISPLAY "LINK-DENNO    = " LINK-DENNO     UPON CONS.
*TÅ™
*
 100-INIT-02.
     OPEN     I-O       SHTDENLJ  BMSHACL3.
*
 100-INIT-03.
*Å@Å@îÑè„ì`ï[ÉtÉ@ÉCÉãÇrÇsÇ`ÇqÇs
     MOVE    SPACE              TO     DEN-REC.
     INITIALIZE                        DEN-REC.
     MOVE    LINK-IN-DATE       TO     DEN-F46.
     MOVE    LINK-IN-TIME       TO     DEN-F47.
     MOVE    LINK-IN-TORICD     TO     DEN-F01.
     MOVE    LINK-IN-FDATE      TO     DEN-F112.
     MOVE    LINK-IN-SOKCD      TO     DEN-F48.
     PERFORM SHTDENLJ-START-SEC.
     IF      SHTDENLJ-INV-FLG    =     "INV"
             MOVE    "END"      TO     SHTDENLJ-END-FLG
             DISPLAY NC"îÑè„ì`ï[ÉtÉ@ÉCÉãÅ@ëŒè€Ç»Çµ" UPON CONS
             GO                 TO     100-INIT-05
     END-IF.
 100-INIT-04.
*Å@Å@îÑè„ì`ï[ÉtÉ@ÉCÉãÇqÇdÇ`Çc
     PERFORM SHTDENLJ-READ-SEC.
     IF      SHTDENLJ-END-FLG    =     "END"
             DISPLAY NC"îÑè„ì`ï[ÉtÉ@ÉCÉãÅ@ëŒè€Ç»Çµ" UPON CONS
             GO                 TO     100-INIT-05
     END-IF.
 100-INIT-05.
*Å@Å@ÇaÇlÇrî≠íçÇlÇrÇf ÇrÇsÇ`ÇqÇs
     MOVE    SPACE              TO     HAC-REC.
     INITIALIZE                        HAC-REC.
     MOVE    LINK-IN-DATE       TO     HAC-F011.
     MOVE    LINK-IN-TIME       TO     HAC-F012.
     MOVE    LINK-IN-TORICD     TO     HAC-F013.
     MOVE    LINK-IN-SOKCD      TO     HAC-F02.
     MOVE    LINK-IN-FDATE      TO     HAC-F346.
     PERFORM BMSHACL3-START-SEC.
     IF      BMSHACL3-INV-FLG    =     "INV"
             MOVE    "END"      TO     BMSHACL3-END-FLG
             DISPLAY NC"ÇaÇlÇrî≠íçÉfÅ[É^Å@ëŒè€Ç»Çµ" UPON CONS
             GO                 TO     100-INIT-RTN-EXIT
     END-IF.
 100-INIT-06.
*Å@Å@ÇaÇlÇrî≠íçÇlÇrÇf ÇqÇdÇ`Çc
     PERFORM BMSHACL3-READ-SEC.
     IF      BMSHACL3-END-FLG    =     "END"
             DISPLAY NC"ÇaÇlÇrî≠íçÉfÅ[É^Å@ëŒè€Ç»Çµ" UPON CONS
             GO                 TO     100-INIT-RTN-EXIT
     END-IF.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      “≤› ºÆÿ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     MOVE    "200-MAIN-RTN"       TO   S-NAME.
*
*îÑè„ì`ï[ÉtÉ@ÉCÉãçXêV
 200-MAIN-01.
     IF      SHTDENLJ-END-FLG =  "END"
             GO                   TO   200-MAIN-02
     END-IF.
     IF      SHTDENLJ-END-FLG =  "HIT"
             MOVE     LINK-IN-TDATE   TO   DEN-F112
             MOVE     LINK-IN-TANCD   TO   DEN-F60
             MOVE     SYS-DATEW       TO   DEN-F63
             MOVE     SYS-TIMEW       TO   DEN-F67
             REWRITE  DEN-REC
             ADD      1               TO   DEN-CNT
             PERFORM  SHTDENLJ-READ-SEC
     END-IF.
     GO                           TO   200-MAIN-01.
*
*ÇaÇlÇrî≠íçÇlÇrÇfçXêV
 200-MAIN-02.
     IF      BMSHACL3-END-FLG =  "END"
             GO                   TO   200-MAIN-EXIT
     END-IF.
     IF      BMSHACL3-END-FLG =  "HIT"
             MOVE     LINK-IN-TDATE   TO   HAC-F346
             REWRITE  HAC-REC
             ADD      1               TO   HAC-CNT
             PERFORM  BMSHACL3-READ-SEC
     END-IF.
     GO                           TO   200-MAIN-02.
*
 200-MAIN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ¥›ƒﬁ ºÆÿ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     MOVE    "300-END-RTN"        TO   S-NAME.
*
     CLOSE    SHTDENLJ  BMSHACL3.
     DISPLAY  NC"îÑè„ì`ï[ÉtÉ@ÉCÉãçXêVÅÅ" DEN-CNT NC"åè"
                                                     UPON CONS.
     DISPLAY  NC"ÇaÇlÇrî≠íçÉfÅ[É^çXêVÅÅ" HAC-CNT NC"åè"
                                                     UPON CONS.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    îÑè„ì`ï[ÉtÉ@ÉCÉãÇrÇsÇ`ÇqÇs
*--------------------------------------------------------------*
 SHTDENLJ-START-SEC      SECTION.
     MOVE    "SHTDENLJ-START-SEC"  TO   S-NAME.
*
     START   SHTDENLJ  KEY  IS  >=     DEN-F46
                                       DEN-F47
                                       DEN-F01
                                       DEN-F112
                                       DEN-F48
                                       DEN-F02
                                       DEN-F04
                                       DEN-F051
                                       DEN-F07
                                       DEN-F03
             INVALID
                       MOVE   "INV"    TO   SHTDENLJ-INV-FLG
             NOT INVALID
                       MOVE   "   "    TO   SHTDENLJ-INV-FLG
     END-START.
*
 SHTDENLJ-START-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    îÑè„ì`ï[ÉtÉ@ÉCÉãÇqÇdÇ`Çc
*--------------------------------------------------------------*
 SHTDENLJ-READ-SEC      SECTION.
     MOVE  "SHTDENLJ-READ-SEC"     TO  S-NAME.
*
 SHTDENLJ-READ-01.
     READ   SHTDENLJ
               AT  END
                        MOVE "END" TO  SHTDENLJ-END-FLG
               NOT AT END
                        MOVE "   " TO  SHTDENLJ-END-FLG
     END-READ.
*
*îÕàÕÉ`ÉFÉbÉN
 SHTDENLJ-READ-02.
     IF   ( DEN-F46   =   LINK-IN-DATE )   AND
          ( DEN-F47   =   LINK-IN-TIME )   AND
          ( DEN-F01   =   LINK-IN-TORICD )
            CONTINUE
     ELSE
            MOVE    "END"          TO  SHTDENLJ-END-FLG
            GO                     TO  SHTDENLJ-READ-EXIT
     END-IF.
*
*î[ïiì˙àÍívÉ`ÉFÉbÉN
 SHTDENLJ-READ-03.
     IF     DEN-F112       =   LINK-IN-FDATE
            CONTINUE
     ELSE
            MOVE    "END"          TO  SHTDENLJ-END-FLG
            GO                     TO  SHTDENLJ-READ-EXIT
     END-IF.
*
*ëqå…àÍívÉ`ÉFÉbÉN
 SHTDENLJ-READ-04.
     IF     LINK-IN-SOKCD  =  SPACE
            CONTINUE
     ELSE
            IF   DEN-F48   =   LINK-IN-SOKCD
                 CONTINUE
            ELSE
                 MOVE    "END"          TO  SHTDENLJ-END-FLG
                 GO                     TO  SHTDENLJ-READ-EXIT
            END-IF
     END-IF.
*
*ì`ï[î‘çÜàÍívÉ`ÉFÉbÉN
 SHTDENLJ-READ-05.
     MOVE "   "            TO               SHTDENLJ-END-FLG.
     IF   LINK-IN-DENNO  = ALL "0"
          MOVE   "HIT"     TO               SHTDENLJ-END-FLG
          GO               TO               SHTDENLJ-READ-EXIT
     END-IF.
     MOVE      LINK-IN-DENNO            TO  WK-DEN.
     PERFORM   VARYING   IX    FROM  1  BY  1  UNTIL  IX  >  20
               IF   WK-DENNO(IX)   =    ZERO
                    CONTINUE
               ELSE
                    IF   DEN-F02   =    WK-DENNO(IX)
                         MOVE   "HIT"   TO  SHTDENLJ-END-FLG
                    END-IF
               END-IF
     END-PERFORM.
     IF   SHTDENLJ-END-FLG  =  "HIT"
          GO            TO      SHTDENLJ-READ-EXIT
     ELSE
          GO            TO      SHTDENLJ-READ-01
     END-IF.
*
 SHTDENLJ-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    ÇaÇlÇrî≠íçÇlÇrÇfÅ@ÇrÇsÇ`ÇqÇs
*--------------------------------------------------------------*
 BMSHACL3-START-SEC      SECTION.
     MOVE    "BMSHACL3-START-SEC"  TO   S-NAME.
*
     START   BMSHACL3  KEY  IS  >=     HAC-F011
                                       HAC-F012
                                       HAC-F013
                                       HAC-F02
                                       HAC-F346
                                       HAC-F308
                                       HAC-F302
                                       HAC-F402
             INVALID
                       MOVE   "INV"    TO   BMSHACL3-INV-FLG
             NOT INVALID
                       MOVE   "   "    TO   BMSHACL3-INV-FLG
     END-START.
*
 BMSHACL3-START-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    ÇaÇlÇrî≠íçÇlÇrÇfÅ@ÇqÇdÇ`Çc
*--------------------------------------------------------------*
 BMSHACL3-READ-SEC      SECTION.
     MOVE  "BMSHACL3-READ-SEC"     TO  S-NAME.
*
 BMSHACL3-READ-01.
     READ   BMSHACL3
               AT  END
                        MOVE "END" TO  BMSHACL3-END-FLG
               NOT AT END
                        MOVE "   " TO  BMSHACL3-END-FLG
     END-READ.
*
*îÕàÕÉ`ÉFÉbÉN
 BMSHACL3-READ-02.
     IF   ( HAC-F011  =   LINK-IN-DATE )   AND
          ( HAC-F012  =   LINK-IN-TIME )   AND
          ( HAC-F013  =   LINK-IN-TORICD )
            CONTINUE
     ELSE
            MOVE    "END"          TO  BMSHACL3-END-FLG
            GO                     TO  BMSHACL3-READ-EXIT
     END-IF.
*
*ëqå…àÍívÉ`ÉFÉbÉN
 BMSHACL3-READ-03.
     IF     LINK-IN-SOKCD  =  SPACE
            CONTINUE
     ELSE
            IF   HAC-F02   =   LINK-IN-SOKCD
                 CONTINUE
            ELSE
                 MOVE    "END"          TO  BMSHACL3-END-FLG
                 GO                     TO  BMSHACL3-READ-EXIT
            END-IF
     END-IF.
*
*î[ïiì˙àÍívÉ`ÉFÉbÉN
 BMSHACL3-READ-04.
     IF     HAC-F346       =   LINK-IN-FDATE
            CONTINUE
     ELSE
            IF       LINK-IN-SOKCD NOT =    SPACE
                     MOVE    "END"     TO   BMSHACL3-END-FLG
                     GO                TO   BMSHACL3-READ-EXIT
            ELSE
                     GO                TO   BMSHACL3-READ-01
            END-IF
     END-IF.
*
*ì`ï[î‘çÜàÍívÉ`ÉFÉbÉN
 BMSHACL3-READ-05.
     MOVE "   "            TO               BMSHACL3-END-FLG.
     IF   LINK-IN-DENNO  = ALL "0"
          MOVE   "HIT"     TO               BMSHACL3-END-FLG
          GO               TO               BMSHACL3-READ-EXIT
     END-IF.
     MOVE      LINK-IN-DENNO            TO  WK-DEN.
     PERFORM   VARYING   IX    FROM  1  BY  1  UNTIL  IX  >  20
               IF   WK-DENNO(IX)   =    ZERO
                    CONTINUE
               ELSE
                    IF   HAC-F302(1:9) =    WK-DENNO(IX)
                         MOVE   "HIT"   TO  BMSHACL3-END-FLG
                    END-IF
               END-IF
     END-PERFORM.
     IF   BMSHACL3-END-FLG  =  "HIT"
          GO            TO      BMSHACL3-READ-EXIT
     ELSE
          GO            TO      BMSHACL3-READ-01
     END-IF.
*
*
 BMSHACL3-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
