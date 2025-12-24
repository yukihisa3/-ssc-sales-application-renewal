****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　発注管理（自動）                  *
*    業務名　　　　　　　：　自動発注数計算                    *
*    モジュール名　　　　：　自動発注数計算確認画面            *
*    作成日／更新日　　　：　2000/07/05                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　画面より、自動発注計算開始の確認を*
*                            行い、抽出条件の定番区分／季節区分*
*                            を入力し、自動発注処理を開始する。*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SHA0100I.
 AUTHOR.                T.TAKAHASHI.
 DATE-WRITTEN.          00/07/05.
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
*---<<  条件ファイル  >>---*
     SELECT   HJYOKEN   ASSIGN    TO   DA-01-VI-JYOKEN1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  JYO-F01
                                       JYO-F02
                        FILE   STATUS  JYO-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 表示ファイル >>--*
 FD  DSPFILE            LABEL     RECORD   IS   STANDARD.
     COPY     FHA01001  OF        XMDLIB.
*---<<  条件ファイル  >>---*
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  ERR-FLG        PIC  9(02)    VALUE  ZERO.
     03  HJYOKEN-INV-FLG PIC X(03)    VALUE  SPACE.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  STATUS-AREA.
     03  JYO-ST         PIC  X(02).
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
              NC"定番区分を正しく入力して下さい。".
     03  MSG03               PIC  N(20)  VALUE
              NC"最初の季節区分は必ず入力して下さい。".
     03  MSG04               PIC  N(20)  VALUE
              NC"季節区分が条件マスタに存在しません。".
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
 01  PARA-TEIBAN            PIC   9(01).
 01  PARA-KISETU1           PIC   X(01).
 01  PARA-KISETU2           PIC   X(01).
 01  PARA-KISETU3           PIC   X(01).
 01  PARA-JIDO              PIC   X(01).
*
****************************************************************
 PROCEDURE              DIVISION USING PARA-TEIBAN  PARA-KISETU1
                                       PARA-KISETU2 PARA-KISETU3
                                       PARA-JIDO.
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
     DISPLAY  "### SHA0100I DSPFILE ERROR " DSP-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    DSPFILE HJYOKEN.
     STOP     RUN.
*----<< 条件ファイル >>--*
 HJYOKEN-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HJYOKEN.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SHA0100I HJYOKEN ERROR " JYO-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    DSPFILE HJYOKEN.
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
     DISPLAY  "*** SHA0100I START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     I-O       DSPFILE.
     OPEN     I-O       HJYOKEN.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     MOVE     0              TO   GR-NO.
*自動発注済区分チェック
     MOVE     "82"     TO   JYO-F01
     MOVE     "JIDO"   TO   JYO-F02
     PERFORM   HJYOKEN-READ-SEC
     IF  HJYOKEN-INV-FLG = SPACE
         IF  JYO-F04  =  1
             MOVE      1        TO   PARA-JIDO
             DISPLAY "## ｼﾞﾄﾞｳﾊｯﾁｭｳ ｽﾞﾐ ##" UPON CONS
             MOVE      99       TO   GR-NO
         END-IF
         REWRITE  JYO-REC
     END-IF.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     MOVE    "200-MAIN-RTN"       TO   S-NAME.
*----<< ｸﾌﾞﾝﾆｭｳﾘｮｸ ｶﾞﾒﾝ ｸﾘｱ >>-*
     PERFORM  210-DSP-INIT   UNTIL     GR-NO    NOT  =    0.
*----<< ｸﾌﾞﾝ    ﾆｭｳﾘｮｸ >>-*
     PERFORM  220-INP-GRP01  UNTIL     GR-NO    NOT  =    1.
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
     DISPLAY  "*** SHA0100I END *** "
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
     MOVE     SPACE          TO   FHA01001.
*画面クリア
     PERFORM  CLR-BODY-RTN.
     PERFORM  CLR-TAIL-RTN.
*ＰＧＩＤ，ＦＯＲＭＩＤセット
     MOVE    "SHA0100I"      TO   PGID.
     MOVE    "FHA01001"      TO   FORM.
*季節区分初期化
     MOVE     SPACE          TO   KISET1.
     MOVE     SPACE          TO   KISET2.
     MOVE     SPACE          TO   KISET3.
*画面初期表示
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     "FHA01001"     TO   DSP-FMT.
     MOVE     "SCREEN"       TO   DSP-GRP.
     PERFORM  900-DSP-WRITE.
*
     MOVE     1              TO   GR-NO.
 210-DSP-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      定番区分／季節区分入力                      *
*--------------------------------------------------------------*
 220-INP-GRP01          SECTION.
     MOVE     "220-INP-GRP01"     TO   S-NAME.
*
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
              PERFORM   220-GRP01-CHECK-SEC
              IF        ERR-FLG   =    ZERO
                        MOVE   9  TO   GR-NO
              END-IF
         WHEN OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 220-INP-GRP01-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      定番区分／季節区分チェック                  *
*--------------------------------------------------------------*
 220-GRP01-CHECK-SEC    SECTION.
     MOVE     "220-CRP01-CHECK-SEC"    TO   S-NAME.
     MOVE     ZERO      TO        ERR-FLG.
*    定番区分チェック
     IF     ( TEIBAN     NOT  NUMERIC   )
              MOVE      2    TO   ERR-FLG
              MOVE     "C"   TO   EDIT-CURSOR OF TEIBAN
              MOVE     "R"   TO   EDIT-OPTION OF TEIBAN
     ELSE
              IF  TEIBAN  =  0  OR  1
                  MOVE     SPACE TO   EDIT-CURSOR OF TEIBAN
                  MOVE     "M"   TO   EDIT-OPTION OF TEIBAN
              ELSE
                  MOVE      2    TO   ERR-FLG
                  MOVE     "C"   TO   EDIT-CURSOR OF TEIBAN
                  MOVE     "R"   TO   EDIT-OPTION OF TEIBAN
              END-IF
     END-IF.
*    季節区分１チェック
     IF   KISET1  NOT =  SPACE
          MOVE     "81"     TO   JYO-F01
          MOVE      KISET1  TO   JYO-F02
          PERFORM   HJYOKEN-READ-SEC
          IF  HJYOKEN-INV-FLG = SPACE
              MOVE     SPACE TO   EDIT-CURSOR OF KISET1
              MOVE     "M"   TO   EDIT-OPTION OF KISET1
              MOVE  JYO-F14  TO   KISEN1
          ELSE
              IF   ERR-FLG  =  ZERO
                   MOVE      4    TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF KISET1
              MOVE     "R"   TO   EDIT-OPTION OF KISET1
              MOVE    SPACE  TO   KISEN1
          END-IF
     ELSE
          IF   ERR-FLG  =  ZERO
               MOVE      3    TO   ERR-FLG
          END-IF
          MOVE         "C"   TO   EDIT-CURSOR OF KISET1
          MOVE         "R"   TO   EDIT-OPTION OF KISET1
          MOVE        SPACE  TO   KISEN1
          GO                 TO   220-GRP01-CHECK-EXIT
     END-IF.
*    季節区分２チェック
     IF   KISET2  NOT =  SPACE
          MOVE     "81"     TO   JYO-F01
          MOVE      KISET2  TO   JYO-F02
          PERFORM   HJYOKEN-READ-SEC
          IF  HJYOKEN-INV-FLG = SPACE
              MOVE     SPACE TO   EDIT-CURSOR OF KISET2
              MOVE     "M"   TO   EDIT-OPTION OF KISET2
              MOVE  JYO-F14  TO   KISEN2
          ELSE
              IF   ERR-FLG  =  ZERO
                   MOVE      4    TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF KISET2
              MOVE     "R"   TO   EDIT-OPTION OF KISET2
              MOVE    SPACE  TO   KISEN2
          END-IF
     ELSE
          MOVE        SPACE  TO   KISEN2
     END-IF.
*    季節区分３チェック
     IF   KISET3  NOT =  SPACE
          MOVE     "81"     TO   JYO-F01
          MOVE      KISET3  TO   JYO-F02
          PERFORM   HJYOKEN-READ-SEC
          IF  HJYOKEN-INV-FLG = SPACE
              MOVE     SPACE TO   EDIT-CURSOR OF KISET3
              MOVE     "M"   TO   EDIT-OPTION OF KISET3
              MOVE  JYO-F14  TO   KISEN3
          ELSE
              IF   ERR-FLG  =  ZERO
                   MOVE      4    TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF KISET3
              MOVE     "R"   TO   EDIT-OPTION OF KISET3
              MOVE    SPACE  TO   KISEN3
          END-IF
     ELSE
          MOVE        SPACE  TO   KISEN3
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
              MOVE      1         TO   GR-NO
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
*定番区分
     MOVE     TEIBAN              TO   PARA-TEIBAN.
*季節区分
     MOVE     KISET1              TO   PARA-KISETU1.
     MOVE     KISET2              TO   PARA-KISETU2.
     MOVE     KISET3              TO   PARA-KISETU3.
*自動発注済区分チェック
     MOVE     "82"     TO   JYO-F01
     MOVE     "JIDO"   TO   JYO-F02
     PERFORM   HJYOKEN-READ-SEC
     IF  HJYOKEN-INV-FLG = SPACE
         MOVE  1                TO   JYO-F04
         REWRITE  JYO-REC
     ELSE
         MOVE  SPACE            TO   JYO-REC
         INITIALIZE                  JYO-REC
         MOVE  "82"             TO   JYO-F01
         MOVE  "JIDO"           TO   JYO-F02
         MOVE  NC"自動発注区分" TO   JYO-F03
         MOVE  1                TO   JYO-F04
         WRITE JYO-REC
     END-IF.
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
     WRITE    FHA01001.
 900-DSP-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＢＯＤＹ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-BODY-RTN           SECTION.
     MOVE     "CLR-BODY-RTN" TO   S-NAME.
     MOVE     " "            TO   EDIT-CURSOR OF TEIBAN.
     MOVE     "M"            TO   EDIT-OPTION OF TEIBAN.
     MOVE     " "            TO   EDIT-CURSOR OF KISET1.
     MOVE     "M"            TO   EDIT-OPTION OF KISET1.
     MOVE     " "            TO   EDIT-CURSOR OF KISET2.
     MOVE     "M"            TO   EDIT-OPTION OF KISET2.
     MOVE     " "            TO   EDIT-CURSOR OF KISET3.
     MOVE     "M"            TO   EDIT-OPTION OF KISET3.
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
*--------------------------------------------------------------*
*    LEVEL  4      条件ファイル読込み                          *
*--------------------------------------------------------------*
 HJYOKEN-READ-SEC       SECTION.
*
     MOVE     "HJYOKEN-READ-SEC" TO   S-NAME.
     READ  HJYOKEN
           INVALID     MOVE  "INV"  TO   HJYOKEN-INV-FLG
           NOT INVALID MOVE   SPACE TO   HJYOKEN-INV-FLG
     END-READ.
*
 HJYOKEN-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
