****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理                          *
*    業務名　　　　　　　：　オンライン管理                    *
*    モジュール名　　　　：　伝票出力順選択                    *
*    作成日／更新日　　　：　07/09/06                          *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　画面より、伝票出力順を選択し，チェ*
*                            ック後，パラメタに渡す。          *
*                            （納品シールバージョン）          *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SKY2006I.
 AUTHOR.                T.TAKAHASHI.
 DATE-WRITTEN.          99/12/06.
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
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 表示ファイル >>--*
 FD  DSPFILE            LABEL     RECORD   IS   STANDARD.
     COPY     FKY20061  OF        XMDLIB.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  ERR-FLG        PIC  9(02)    VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
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
              NC"伝票出力順を入力して下さい。".
     03  MSG03               PIC  N(20)  VALUE
              NC"開始納品日を指定して下さい。".
     03  MSG04               PIC  N(20)  VALUE
              NC"終了納品日を指定して下さい。".
     03  MSG05               PIC  N(20)  VALUE
              NC"開始が終了を越えています。".
*
 01  FILLER                  REDEFINES   MSG-AREA.
     03  MSG-TBL             PIC  N(20)  OCCURS       5.
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
 01  PARA-SOKCD             PIC   9(02).
 01  PARA-KBN               PIC   9(01).
 01  PARA-NOUDT1            PIC   9(08).
 01  PARA-NOUDT2            PIC   9(08).
 01  PARA-TENCD1            PIC   9(05).
 01  PARA-TENCD2            PIC   9(05).
 01  PARA-BUMON1            PIC   X(04).
 01  PARA-BUMON2            PIC   X(04).
*
****************************************************************
 PROCEDURE              DIVISION USING PARA-SOKCD  PARA-KBN
                                       PARA-NOUDT1 PARA-NOUDT2
                                       PARA-TENCD1 PARA-TENCD2
                                       PARA-BUMON1 PARA-BUMON2.
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
     DISPLAY  "### SKY2006I DSPFILE ERROR " DSP-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    DSPFILE.
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
     DISPLAY  "*** SKY2006I START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     I-O       DSPFILE.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     MOVE     0              TO   GR-NO.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     MOVE    "200-MAIN-RTN"       TO   S-NAME.
*----<< ﾊﾝｲ ｼﾃｲ ｶﾞﾒﾝ ｸﾘｱ >>-*
     PERFORM  210-DSP-INIT   UNTIL     GR-NO    NOT  =    0.
*----<< ｸﾌﾞﾝ    ﾆｭｳﾘｮｸ >>-*
     PERFORM  220-INP-GRP01  UNTIL     GR-NO    NOT  =    1.
*----<< ﾉｳﾋﾝﾋﾞ  ﾆｭｳﾘｮｸ >>-*
     PERFORM  220-INP-GRP02  UNTIL     GR-NO    NOT  =    2.
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
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SKY2006I END *** "
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
     MOVE     SPACE          TO   FKY20061.
*
     PERFORM  CLR-HEAD-RTN.
     PERFORM  CLR-BODY-RTN.
     PERFORM  CLR-TAIL-RTN.
*
     MOVE    "SKY2006I"      TO   PGID.
     MOVE    "FKY20061"      TO   FORM.
*    パラメタ倉庫コードセット
     MOVE    PARA-SOKCD      TO   SOKCD.
*    出力区分セット
     MOVE    4               TO   KBN.
*
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     "FKY20061"     TO   DSP-FMT.
     MOVE     "SCREEN"       TO   DSP-GRP.
     PERFORM  900-DSP-WRITE.
*
     MOVE     1              TO   GR-NO.
 210-DSP-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      伝票出力順入力                              *
*--------------------------------------------------------------*
 220-INP-GRP01          SECTION.
     MOVE     "220-INP-GRP01"     TO   S-NAME.
     MOVE     "GRP001"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF04
              MOVE      0         TO   GR-NO
         WHEN PF05
              MOVE      99        TO   GR-NO
              MOVE      4010      TO   PROGRAM-STATUS
         WHEN PF06
              MOVE      1         TO   GR-NO
         WHEN ENT
              PERFORM   CLR-HEAD-RTN
              PERFORM   220-GRP01-CHECK-SEC
              IF        ERR-FLG   =    ZERO
                        IF  KBN = 1
                            MOVE  9    TO   GR-NO
                        ELSE
                            MOVE  2    TO   GR-NO
                        END-IF
              END-IF
         WHEN OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 220-INP-GRP01-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      倉庫ＣＤチェック                            *
*--------------------------------------------------------------*
 220-GRP01-CHECK-SEC    SECTION.
     MOVE     "220-CRP01-CHECK-SEC"    TO   S-NAME.
     MOVE     ZERO      TO        ERR-FLG.
*    伝票出力順チェック
     IF     ( KBN        NOT  NUMERIC   )
     OR     ( KBN        =    ZERO      )
              MOVE      2    TO   ERR-FLG
              MOVE     "C"   TO   EDIT-CURSOR OF KBN
              MOVE     "R"   TO   EDIT-OPTION OF KBN
     ELSE
              IF  KBN  =  1  OR  2  OR  3  OR  4
                  MOVE     SPACE TO   EDIT-CURSOR OF KBN
                  MOVE     "M"   TO   EDIT-OPTION OF KBN
              ELSE
                  MOVE      2    TO   ERR-FLG
                  MOVE     "C"   TO   EDIT-CURSOR OF KBN
                  MOVE     "R"   TO   EDIT-OPTION OF KBN
              END-IF
     END-IF.
*
 220-GRP01-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4      納品日入力（範囲）                          *
*--------------------------------------------------------------*
 220-INP-GRP02          SECTION.
     MOVE     "220-INP-GRP02"     TO   S-NAME.
     MOVE     "GRP002"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF04
              MOVE      0         TO   GR-NO
         WHEN PF05
              MOVE      99        TO   GR-NO
              MOVE      4010      TO   PROGRAM-STATUS
         WHEN PF06
              MOVE      1         TO   GR-NO
         WHEN ENT
              PERFORM   CLR-BODY-RTN
              PERFORM   220-GRP02-CHECK-SEC
              IF        ERR-FLG   =    ZERO
                        MOVE      9    TO   GR-NO
              END-IF
         WHEN OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 220-INP-GRP02-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4      納品日範囲チェック                          *
*--------------------------------------------------------------*
 220-GRP02-CHECK-SEC    SECTION.
     MOVE     "220-CRP02-CHECK-SEC"    TO   S-NAME.
     MOVE     ZERO      TO        ERR-FLG.
*開始納品日未入力時（初期値セット）
     IF       NOUDT1  NOT  NUMERIC
              MOVE     ZERO  TO   NOUDT1
     END-IF.
*終了納品日未入力時（初期値セット）
     IF       NOUDT2  NOT  NUMERIC
              MOVE  99999999 TO   NOUDT2
     END-IF.
*納品日範囲大小チェック
     IF       NOUDT1  >  NOUDT2
              IF    ERR-FLG = ZERO
                    MOVE      5    TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF NOUDT1
              MOVE     "R"   TO   EDIT-OPTION OF NOUDT1
              MOVE     "R"   TO   EDIT-OPTION OF NOUDT2
     ELSE
              MOVE     SPACE TO   EDIT-CURSOR OF NOUDT1
              MOVE     "M"   TO   EDIT-OPTION OF NOUDT1
              MOVE     "M"   TO   EDIT-OPTION OF NOUDT2
     END-IF.
*日付論理チェック（開始納品日）
     IF       NOUDT1  NOT =  ZERO
              MOVE    "2"        TO        LINK-IN-KBN
              MOVE     NOUDT1    TO        LINK-IN-YMD8
              CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                           LINK-IN-YMD6
                                           LINK-IN-YMD8
                                           LINK-OUT-RET
                                           LINK-OUT-YMD8
              IF       LINK-OUT-RET   =    ZERO
                       MOVE SPACE  TO   EDIT-CURSOR OF NOUDT1
                       MOVE "M"    TO   EDIT-OPTION OF NOUDT1
              ELSE
                       IF  ERR-FLG = ZERO
                           MOVE      3    TO   ERR-FLG
                       END-IF
                       MOVE "C"    TO   EDIT-CURSOR OF NOUDT1
                       MOVE "R"    TO   EDIT-OPTION OF NOUDT1
              END-IF
     END-IF.
*日付論理チェック（終了納品日）
     IF       NOUDT2  NOT =  99999999
              MOVE    "2"        TO        LINK-IN-KBN
              MOVE     NOUDT2    TO        LINK-IN-YMD8
              CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                           LINK-IN-YMD6
                                           LINK-IN-YMD8
                                           LINK-OUT-RET
                                           LINK-OUT-YMD8
              IF       LINK-OUT-RET   =    ZERO
                       MOVE SPACE  TO   EDIT-CURSOR OF NOUDT2
                       MOVE "M"    TO   EDIT-OPTION OF NOUDT2
              ELSE
                       IF  ERR-FLG = ZERO
                           MOVE      4    TO   ERR-FLG
                       END-IF
                       MOVE "C"    TO   EDIT-CURSOR OF NOUDT2
                       MOVE "R"    TO   EDIT-OPTION OF NOUDT2
              END-IF
     END-IF.
*開始店舗未入力時（初期値セット）
     IF       TENCD1  NOT  NUMERIC
              MOVE  ZERO     TO   TENCD1
     END-IF.
*終了店舗未入力時（初期値セット）
     IF       TENCD2  NOT  NUMERIC
              MOVE  99999    TO   TENCD2
     END-IF.
*納品日範囲大小チェック
     IF       TENCD1  >  TENCD2
              IF    ERR-FLG = ZERO
                    MOVE      5    TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF TENCD1
              MOVE     "R"   TO   EDIT-OPTION OF TENCD1
              MOVE     "R"   TO   EDIT-OPTION OF TENCD1
     ELSE
              MOVE     SPACE TO   EDIT-CURSOR OF TENCD1
              MOVE     "M"   TO   EDIT-OPTION OF TENCD1
              MOVE     "M"   TO   EDIT-OPTION OF TENCD2
     END-IF.
*開始部門未入力時（初期値セット）
     IF       BUMON1  =  SPACE
              MOVE  SPACE    TO   BUMON1
     END-IF.
*終了部門未入力時（初期値セット）
     IF       BUMON2  =  SPACE
              MOVE  "9999"   TO   BUMON2
     END-IF.
*納品日範囲大小チェック
     IF       BUMON1  >  BUMON2
              IF    ERR-FLG = ZERO
                    MOVE      5    TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF BUMON1
              MOVE     "R"   TO   EDIT-OPTION OF BUMON1
              MOVE     "R"   TO   EDIT-OPTION OF BUMON1
     ELSE
              MOVE     SPACE TO   EDIT-CURSOR OF BUMON1
              MOVE     "M"   TO   EDIT-OPTION OF BUMON1
              MOVE     "M"   TO   EDIT-OPTION OF BUMON2
     END-IF.
*
 220-GRP01-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｶｸﾆﾝ ﾆｭｳﾘｮｸ                                 *
*--------------------------------------------------------------*
 230-INP-KKNN           SECTION.
     MOVE     "230-INP-KKNN"      TO   S-NAME.
     MOVE     "KAKU"         TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
              MOVE      4010      TO   PROGRAM-STATUS
         WHEN PF04
              MOVE      0         TO   GR-NO
         WHEN PF06
              IF        KBN = 1
                        MOVE  1   TO   GR-NO
              ELSE
                        MOVE  2   TO   GR-NO
              END-IF
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
*
     MOVE     KBN                 TO   PARA-KBN.
*
     EVALUATE  KBN
         WHEN  1
               MOVE   ZERO        TO   PARA-NOUDT1
               MOVE   99999999    TO   PARA-NOUDT2
               MOVE   ZERO        TO   PARA-TENCD1
               MOVE   99999       TO   PARA-TENCD2
               MOVE   SPACE       TO   PARA-BUMON1
               MOVE   9999        TO   PARA-BUMON2
         WHEN  2
               MOVE   ZERO        TO   PARA-NOUDT1
               MOVE   99999999    TO   PARA-NOUDT2
               MOVE   TENCD1      TO   PARA-TENCD1
               MOVE   TENCD2      TO   PARA-TENCD2
               MOVE   SPACE       TO   PARA-BUMON1
               MOVE   9999        TO   PARA-BUMON2
         WHEN  3
               MOVE   NOUDT1      TO   PARA-NOUDT1
               MOVE   NOUDT2      TO   PARA-NOUDT2
               MOVE   TENCD1      TO   PARA-TENCD1
               MOVE   TENCD2      TO   PARA-TENCD2
               MOVE   SPACE       TO   PARA-BUMON1
               MOVE   9999        TO   PARA-BUMON2
         WHEN  4
               MOVE   NOUDT1      TO   PARA-NOUDT1
               MOVE   NOUDT2      TO   PARA-NOUDT2
               MOVE   TENCD1      TO   PARA-TENCD1
               MOVE   TENCD2      TO   PARA-TENCD2
               MOVE   BUMON1      TO   PARA-BUMON1
               MOVE   BUMON2      TO   PARA-BUMON2
     END-EVALUATE.

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
     WRITE    FKY20061.
 900-DSP-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＨＥＡＤ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-HEAD-RTN           SECTION.
     MOVE     "CLR-HEAD-RTN" TO   S-NAME.
     MOVE     " "            TO   EDIT-CURSOR OF KBN.
     MOVE     "M"            TO   EDIT-OPTION OF KBN.
 CLR-HEAD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＢＯＤＹ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-BODY-RTN           SECTION.
     MOVE     "CLR-BODY-RTN" TO   S-NAME.
     MOVE     " "            TO   EDIT-CURSOR OF NOUDT1.
     MOVE     "M"            TO   EDIT-OPTION OF NOUDT1.
     MOVE     " "            TO   EDIT-CURSOR OF NOUDT2.
     MOVE     "M"            TO   EDIT-OPTION OF NOUDT2.
     MOVE     " "            TO   EDIT-CURSOR OF TENCD1.
     MOVE     "M"            TO   EDIT-OPTION OF TENCD1.
     MOVE     " "            TO   EDIT-CURSOR OF TENCD2.
     MOVE     "M"            TO   EDIT-OPTION OF TENCD2.
     MOVE     " "            TO   EDIT-CURSOR OF BUMON1.
     MOVE     "M"            TO   EDIT-OPTION OF BUMON1.
     MOVE     " "            TO   EDIT-CURSOR OF BUMON2.
     MOVE     "M"            TO   EDIT-OPTION OF BUMON2.
 CLR-BODY-EXIT.
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
