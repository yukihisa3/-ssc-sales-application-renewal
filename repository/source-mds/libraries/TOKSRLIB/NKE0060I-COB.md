# NKE0060I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NKE0060I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　出荷　　　　　                    *
*    サブシステム　　　　：　検品システム                      *
*    モジュール名　　　　：　出荷検品確定データ取込指示        *
*    作成日　　　　　　　：　2019/01/09                        *
*    作成者　　　　　　　：　INOUE                             *
*    処理概要　　　　　　：　転送された出荷確定データの　　　  *
*                            取込処理を行う。その確認・指示。  *
*    更新日　　　　　　　：　2023/01/18                        *
*    更新者　　　　　　　：　INOUE                             *
*    処理概要　　　　　　：　取込データ区分に選択肢追加　　　  *
*                            　３：ヨドバシ　　　　　　　　　　*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            NKE0060I.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2019/01/09.
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
*----<< 表示ファイル >>--*
     SELECT   DSPFILE   ASSIGN         01-GS-DSPF
                        FORMAT         DSP-FMT
                        GROUP          DSP-GRP
                        PROCESSING     DSP-PRO
                        UNIT CONTROL   DSP-CON
                        FUNCTION       DSP-FNC
                        STATUS         DSP-ST.
*----<< 倉庫マスタ >>--*
     SELECT   ZSOKMS1   ASSIGN         DA-01-VI-ZSOKMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SOK-F01
                        STATUS         SOK-ST.
*----<< 担当者マスタ >>--*
     SELECT   TANMS1    ASSIGN         DA-01-VI-TANMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TAN-F01 TAN-F02
                        STATUS         TAN-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 表示ファイル >>--*
 FD  DSPFILE            LABEL     RECORD   IS   STANDARD.
     COPY     FKE00601  OF        XMDLIB.
*----<< 倉庫マスタ >>--*
 FD  ZSOKMS1            LABEL RECORD   IS   STANDARD.
     COPY     ZSOKMS1   OF        XFDLIB
              JOINING   SOK       PREFIX.
*----<< 担当者マスタ >>--*
 FD  TANMS1             LABEL RECORD   IS   STANDARD.
     COPY     TANMS1    OF        XFDLIB
              JOINING   TAN       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  ERR-FLG        PIC  9(02)    VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SOK-ST            PIC  X(02).
 01  TAN-ST            PIC  X(02).
*
*----<< WORK       >>--*
 01  KENSU              PIC  9(07)     VALUE ZERO.
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
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
 01  SYS-TIME2          PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME2.
     03  SYS-TIMEW      PIC  9(06).
     03  FILLER         PIC  9(02).
*
*----<< ﾃﾞｨｽﾌﾟﾚｲ ｺﾝﾄﾛｰﾙ ｴﾘｱ >>-*
 01  GR-NO              PIC  9(02).
 01  WK-GRP             PIC  X(08).
 01  DSP-CNTL.
     03  DSP-ST         PIC  X(02).
     03  DSP-ST2        PIC  X(04).
     03  DSP-FMT        PIC  X(08).
     03  DSP-GRP        PIC  X(08).
     03  DSP-PRO        PIC  X(02).
     03  DSP-FNC        PIC  X(04).
     03  DSP-CON        PIC  X(06).
*
*----<< ﾌｱﾝｸｼﾖﾝ ｷｰ ﾃ-ﾌﾞﾙ >>-*
 01  FNC-TABLE.
     03  ENT            PIC  X(04)     VALUE     "E000".
     03  PF04           PIC  X(04)     VALUE     "F004".
     03  PF05           PIC  X(04)     VALUE     "F005".
     03  PF06           PIC  X(04)     VALUE     "F006".
*
*----<< ﾌｱﾝｸｼﾖﾝ ｷｰ ｶﾞｲﾄﾞ >>-*
 01  GUIDE01       PIC  N(40)  VALUE   NC"_終　了".
 01  GUIDE02       PIC  N(40)  VALUE
         NC"_取　消　_終　了　_項目戻り".
*
 01  MSG-AREA.
     03  MSG01               PIC  N(20)  VALUE
              NC"ＰＦキーが違います".
     03  MSG02               PIC  N(20)  VALUE
              NC"空白もしくは１のみ指定可能です".
     03  MSG03               PIC  N(20)  VALUE
              NC"出荷確定データはゼロ件です".
     03  MSG04               PIC  N(20)  VALUE
              NC"１、２、３のみ指定可能です".
*
 01  FILLER                  REDEFINES   MSG-AREA.
     03  MSG-TBL             PIC  N(20)  OCCURS       4.
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
 LINKAGE                SECTION.
 01  PARA-IN-BUMON          PIC   X(04).
 01  PARA-IN-TANTOU         PIC   X(02).
 01  PARA-IN-SOKCD          PIC   X(02).
 01  PARA-OUT-DATKBN        PIC   X(01).
 01  PARA-OUT-HAKKBN        PIC   X(01).
 01  PARA-OUT-OUTKBN        PIC   X(01).
 01  PARA-OUT-TDATE         PIC   9(08).
 01  PARA-OUT-TTIME         PIC   9(06).
*
****************************************************************
 PROCEDURE              DIVISION USING PARA-IN-BUMON
                                       PARA-IN-TANTOU
                                       PARA-IN-SOKCD
                                       PARA-OUT-DATKBN
                                       PARA-OUT-HAKKBN
                                       PARA-OUT-OUTKBN
                                       PARA-OUT-TDATE
                                       PARA-OUT-TTIME.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 表示ファイル >>--*
 DSPFILE-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DSPFILE.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### NKE0060I DSPFILE ERROR " DSP-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     STOP     RUN.
*----<< 倉庫マスタ >>--*
 ZSOKMS1-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      ZSOKMS1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### NKE0060I ZSOKMS1 ERROR " SOK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     STOP     RUN.
*----<< 担当者マスタ >>--*
 TANMS1-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      TANMS1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### NKE0060I TANMS1 ERROR " TAN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     MOVE    "000-PROG-CNTL"      TO   S-NAME.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     GR-NO    =    99.
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
     MOVE    "100-INIT-RTN"       TO   S-NAME.
     ACCEPT   SYS-DATE       FROM DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYS-DATE  TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
       DISPLAY "LINK-YMD8 = " LINK-OUT-YMD8 UPON CONS
         MOVE LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE ZERO           TO   SYS-DATEW
     END-IF.
*
     MOVE     SYS-YYW        TO   WK-SYSYY.
     MOVE     SYS-MMW        TO   WK-SYSMM.
     MOVE     SYS-DDW        TO   WK-SYSDD.
*
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** NKE0060I START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     I-O       DSPFILE.
     OPEN     INPUT     ZSOKMS1 TANMS1.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     MOVE     0              TO   GR-NO.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     MOVE    "200-MAIN-RTN"       TO   S-NAME.
*----<< 初期画面表示 >>-*
     PERFORM  210-DSP-INIT   UNTIL     GR-NO    NOT  =    0.
*----<< 取込データ選択　入力 >>-*
     PERFORM  220-INP-GRP01  UNTIL     GR-NO    NOT  =    1.
*----<< 取込リスト発行区分　入力 >>-*
     PERFORM  220-INP-GRP02  UNTIL     GR-NO    NOT  =    2.
*----<< 出力区分　入力 >>-*
     PERFORM  220-INP-GRP03  UNTIL     GR-NO    NOT  =    3.
*----<< ｶｸﾆﾝ ﾆｭｳﾘｮｸ >>-*
     PERFORM  230-INP-KKNN   UNTIL     GR-NO    NOT  =    9.
*----<< ﾊﾟﾗﾒﾀ ｼｭﾂﾘｮｸ >>-*
     PERFORM  240-PARA-SEC   UNTIL     GR-NO    NOT  =    10.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     MOVE    "300-END-RTN"        TO   S-NAME.
     CLOSE    DSPFILE.
     CLOSE    ZSOKMS1 TANMS1.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** NKE0060I END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      画面初期化                                  *
*--------------------------------------------------------------*
 210-DSP-INIT           SECTION.
     MOVE    "210-DSP-INIT"       TO   S-NAME.
     MOVE     SPACE          TO   FKE00601.
*
     PERFORM  CLR-HEAD-RTN.
     PERFORM  CLR-TAIL-RTN.
*
     MOVE    "NKE0060I"      TO   PGID.
     MOVE    "FKE00601"      TO   FORM.
*
     MOVE      PARA-IN-SOKCD TO   SOKCD.
     MOVE      SOKCD         TO   SOK-F01.
     READ      ZSOKMS1
      INVALID  KEY
               MOVE      SPACE     TO   SOKNM
      NOT INVALID  KEY
               MOVE      SPACE     TO   SOKNM
               MOVE      SOK-F02   TO   SOKNM
     END-READ.
*
*    DISPLAY "BUMON  = " PARA-IN-BUMON UPON CONS.
*    DISPLAY "TANTOU = " PARA-IN-TANTOU UPON CONS.
     MOVE     PARA-IN-BUMON       TO   TAN-F01.
     MOVE     PARA-IN-TANTOU      TO   TAN-F02.
     MOVE     PARA-IN-BUMON       TO   TTANCD(1:4).
     MOVE     "-"                 TO   TTANCD(5:1).
     MOVE     PARA-IN-TANTOU      TO   TTANCD(6:2).
     READ     TANMS1
              INVALID  KEY
                        MOVE      SPACE     TO   TANNM
              NOT INVALID  KEY
                        MOVE      SPACE     TO   TANNM
                        MOVE      TAN-F03   TO   TANNM
     END-READ.
*
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     "FKE00601"     TO   DSP-FMT.
     MOVE     "SCREEN"       TO   DSP-GRP.
     PERFORM  900-DSP-WRITE.
*
     MOVE     1              TO   GR-NO.
 210-DSP-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      取込データ選択　　入力                      *
*--------------------------------------------------------------*
 220-INP-GRP01          SECTION.
     MOVE     "220-INP-GRP01"     TO   S-NAME.
     MOVE     "GRP01"        TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
              MOVE      4010      TO   PROGRAM-STATUS
         WHEN ENT
              PERFORM   CLR-HEAD-RTN
              PERFORM   220-GRP01-CHECK-SEC
              IF        ERR-FLG   =    ZERO
*                  IF   DTKBN     =    " "
*                       MOVE      9    TO   GR-NO
*                  ELSE
                        MOVE      2    TO   GR-NO
*                  END-IF
              END-IF
         WHEN OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 220-INP-GRP01-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      取込データ選択　　チェック                  *
*--------------------------------------------------------------*
 220-GRP01-CHECK-SEC    SECTION.
     MOVE     "220-CRP01-CHECK-SEC"    TO   S-NAME.
     MOVE     ZERO      TO        ERR-FLG.
*
     IF     ( DTKBN      NOT =    "1"   )   AND
            ( DTKBN      NOT =    "2"   )   AND
            ( DTKBN      NOT =    "3"   )
              MOVE      4    TO   ERR-FLG
              MOVE     "C"   TO   EDIT-CURSOR OF DTKBN
              MOVE     "R"   TO   EDIT-OPTION OF DTKBN
     ELSE
              MOVE     SPACE TO   EDIT-CURSOR OF DTKBN
              MOVE     "M"   TO   EDIT-OPTION OF DTKBN
     END-IF.
*
 220-GRP01-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      出荷実績出力区分　入力                      *
*--------------------------------------------------------------*
 220-INP-GRP02          SECTION.
     MOVE     "220-INP-GRP02"     TO   S-NAME.
     MOVE     "GRP02"        TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
              MOVE      4010      TO   PROGRAM-STATUS
         WHEN PF06
              MOVE      1         TO   GR-NO
         WHEN ENT
              PERFORM   CLR-HEAD-RTN
              PERFORM   220-GRP02-CHECK-SEC
              IF        ERR-FLG   =    ZERO
*                  IF   THKKBN    =    " "
*                       MOVE      9    TO   GR-NO
*                  ELSE
                        MOVE      3    TO   GR-NO
*                  END-IF
              END-IF
         WHEN OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 220-INP-GRP02-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      出荷実績出力区分　チェック                  *
*--------------------------------------------------------------*
 220-GRP02-CHECK-SEC    SECTION.
     MOVE     "220-CRP02-CHECK-SEC"    TO   S-NAME.
     MOVE     ZERO      TO        ERR-FLG.
*
     IF     ( THKKBN     NOT =    " "   )   AND
            ( THKKBN     NOT =    "1"   )
              MOVE      2    TO   ERR-FLG
              MOVE     "C"   TO   EDIT-CURSOR OF THKKBN
              MOVE     "R"   TO   EDIT-OPTION OF THKKBN
     ELSE
              MOVE     SPACE TO   EDIT-CURSOR OF THKKBN
              MOVE     "M"   TO   EDIT-OPTION OF THKKBN
     END-IF.
*
 220-GRP02-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      出力区分　入力
*--------------------------------------------------------------*
 220-INP-GRP03          SECTION.
     MOVE     "220-INP-GRP03"     TO   S-NAME.
     MOVE     "GRP03"        TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF04
              MOVE      0         TO   GR-NO
         WHEN PF05
              MOVE      99        TO   GR-NO
              MOVE      4010      TO   PROGRAM-STATUS
         WHEN PF06
              MOVE      2         TO   GR-NO
         WHEN ENT
              PERFORM   CLR-HEAD-RTN
              PERFORM   220-GRP03-CHECK-SEC
              IF        ERR-FLG   =    ZERO
                        MOVE      9    TO   GR-NO
              END-IF
         WHEN OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 220-INP-GRP03-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      出力区分　チェック
*--------------------------------------------------------------*
 220-GRP03-CHECK-SEC    SECTION.
     MOVE     "220-CRP03-CHECK-SEC"    TO   S-NAME.
     MOVE     ZERO      TO        ERR-FLG.
*
     IF     ( SYRKBN     NOT =    " "   )   AND
            ( SYRKBN     NOT =    "1"   )
              MOVE      2    TO   ERR-FLG
              MOVE     "C"   TO   EDIT-CURSOR OF SYRKBN
              MOVE     "R"   TO   EDIT-OPTION OF SYRKBN
     ELSE
              MOVE     SPACE TO   EDIT-CURSOR OF SYRKBN
              MOVE     "M"   TO   EDIT-OPTION OF SYRKBN
     END-IF.
*
 220-GRP03-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｶｸﾆﾝ ﾆｭｳﾘｮｸ                                 *
*--------------------------------------------------------------*
 230-INP-KKNN           SECTION.
     MOVE     "230-INP-KKNN"      TO   S-NAME.
     MOVE     "KKNN"         TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
              MOVE      4010      TO   PROGRAM-STATUS
         WHEN PF04
              MOVE      0         TO   GR-NO
         WHEN PF06
              MOVE      3         TO   GR-NO
         WHEN ENT
              PERFORM   CLR-TAIL-RTN
              MOVE      10        TO   GR-NO
         WHEN OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 230-INP-KKNN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾟﾗﾒﾀ ｼｭﾂﾘｮｸ                                *
*--------------------------------------------------------------*
 240-PARA-SEC           SECTION.
     MOVE     "240-PARA-SEC"      TO   S-NAME.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME2      FROM TIME.
     MOVE     "3"            TO   LINK-IN-KBN.
     MOVE     SYS-DATE       TO   LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF      LINK-OUT-RET   =    ZERO
             MOVE LINK-OUT-YMD8  TO   PARA-OUT-TDATE
     ELSE
             MOVE ZERO           TO   PARA-OUT-TDATE
     END-IF.
     MOVE     SYS-TIMEW TO   PARA-OUT-TTIME.
     MOVE     DTKBN     TO   PARA-OUT-DATKBN.
     MOVE     THKKBN    TO   PARA-OUT-HAKKBN.
     MOVE     SYRKBN    TO   PARA-OUT-OUTKBN.
*
     MOVE     99                  TO   GR-NO.
*
 240-PARA-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  READ                              *
*--------------------------------------------------------------*
 900-DSP-READ           SECTION.
     MOVE     "900-DSP-READ"      TO   S-NAME.
     MOVE     "SCREEN"       TO   DSP-GRP.
     IF       ERR-FLG   =    0
              MOVE      SPACE               TO   MSG
     ELSE
              MOVE      MSG-TBL (ERR-FLG)   TO   MSG
     END-IF.
     IF       GR-NO     =    1
              MOVE      GUIDE01   TO   GUIDE
     ELSE
              MOVE      GUIDE02   TO   GUIDE
     END-IF.
     MOVE     WK-SYSYMD           TO   SYSYMD.
     ACCEPT   SYS-TIME2           FROM TIME.
     MOVE     SYS-TIMEW           TO   SYSTIM.
*
     PERFORM  900-DSP-WRITE.
*
     MOVE    "NE"            TO   DSP-PRO.
     MOVE     SPACE          TO   MSG.
*
     MOVE     WK-GRP         TO   DSP-GRP.
     READ     DSPFILE.
     MOVE     SPACE          TO   DSP-PRO.
 900-DSP-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  WRITE                             *
*--------------------------------------------------------------*
 900-DSP-WRITE          SECTION.
     MOVE     "900-DSP-WRITE"     TO   S-NAME.
*****MOVE     SPACE          TO   DSP-PRO.
     WRITE    FKE00601.
 900-DSP-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＨＥＡＤ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-HEAD-RTN           SECTION.
     MOVE     "CLR-HEAD-RTN" TO   S-NAME.
     MOVE     " "            TO   EDIT-CURSOR OF SOKCD.
     MOVE     "M"            TO   EDIT-OPTION OF SOKCD.
 CLR-HEAD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＴＡＩＬ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-TAIL-RTN           SECTION.
     MOVE     "CLR-TAIL-RTN" TO   S-NAME.
     MOVE     " "            TO   EDIT-CURSOR OF KKNN.
     MOVE     "M"            TO   EDIT-OPTION OF KKNN.
 CLR-TAIL-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
