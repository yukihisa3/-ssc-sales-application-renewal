****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　種子実績データ確認　　　　　　　  *
*    サブシステム　　　　：　種子実績システム　　　　　　　　  *
*    モジュール名　　　　：　注文日別受注ＤＴ抽出指示画面　　　*
*    作成日／作成者　　　：　2023/07/20 TAKAHASHI              *
*    処理概要　　　　　　：　画面より注文日別受注ＤＴ抽出を　　*
*                            行なう条件を入力する。　　　　　  *
*                            チェック後、パラメタへ渡す。      *
*    更新日／更新者　　　：　                                  *
*    更新内容　　　　　　：　　　　　　　　　　　　　　　　　　*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            CHU0010I.
*                  流用:SSY9480I.TOKSRLIB
 AUTHOR.                NAV.
 DATE-WRITTEN.          2023/01/17.
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
*----<< 条件ファイル >>--*
     SELECT   HJYOKEN   ASSIGN         DA-01-VI-JYOKEN1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  JYO-F01  JYO-F02
                        STATUS         JYO-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 表示ファイル >>--*
 FD  DSPFILE            LABEL     RECORD   IS   STANDARD.
     COPY     FHU00101  OF        XMDLIB.
*----<< 条件ファイル >>--*
 FD  HJYOKEN            LABEL RECORD   IS   STANDARD.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  ERR-FLG        PIC  9(02)    VALUE  ZERO.
     03  HJYOKEN-INV-FLG PIC  X(03)   VALUE  SPACE.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  JYO-ST            PIC  X(02).
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
 01  WK-SOKCD           PIC  X(02)  VALUE  SPACE.
 01  WK-YODOBASI          PIC  9(08)  VALUE  ZERO.
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
              NC"開始が終了を超えています。".
     03  MSG03               PIC  N(20)  VALUE
              NC"指定した商品カテゴリがマスタ未登録です。".
     03  MSG04               PIC  N(20)  VALUE
              NC"指定した分類区分がマスタ未登録です。".
     03  MSG05               PIC  N(20)  VALUE
              NC"抽出区分の指定が違います。".
     03  MSG06               PIC  N(20)  VALUE
              NC"日付論理エラーです。".
     03  MSG07               PIC  N(20)  VALUE
              NC"同一の商品カテゴリが指定されています。".
     03  MSG08               PIC  N(20)  VALUE
              NC"同一の分類区分が指定されています。".
     03  MSG09               PIC  N(20)  VALUE
              NC"　".
     03  MSG10               PIC  N(20)  VALUE
              NC"　".
     03  MSG11               PIC  N(20)  VALUE
              NC"　".
     03  MSG12               PIC  N(20)  VALUE
              NC"　".
*
 01  FILLER                  REDEFINES   MSG-AREA.
     03  MSG-TBL             PIC  N(20)  OCCURS      12.
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
 01  PARA-TKBN              PIC   X(01).
 01  PARA-STTOKCD           PIC   9(08).
 01  PARA-EDTOKCD           PIC   9(08).
 01  PARA-STDT              PIC   9(08).
 01  PARA-EDDT              PIC   9(08).
 01  PARA-STSOKCD           PIC   X(02).
 01  PARA-EDSOKCD           PIC   X(02).
 01  PARA-CATEG1            PIC   X(02).
 01  PARA-CATEG2            PIC   X(02).
 01  PARA-CATEG3            PIC   X(02).
 01  PARA-BUNRU1            PIC   X(01).
 01  PARA-BUNRU2            PIC   X(01).
 01  PARA-BUNRU3            PIC   X(01).
*
****************************************************************
 PROCEDURE              DIVISION USING PARA-TKBN
                                       PARA-STTOKCD
                                       PARA-EDTOKCD
                                       PARA-STDT
                                       PARA-EDDT
                                       PARA-STSOKCD
                                       PARA-EDSOKCD
                                       PARA-CATEG1
                                       PARA-CATEG2
                                       PARA-CATEG3
                                       PARA-BUNRU1
                                       PARA-BUNRU2
                                       PARA-BUNRU3.
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
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### CHU0010I DSPFILE ERROR " DSP-CNTL " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    HJYOKEN    DSPFILE.
     STOP     RUN.
*----<< 条件ファイル >>--*
 HJYOKEN-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HJYOKEN.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### CHU0010I HJYOKEN ERROR " JYO-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    HJYOKEN    DSPFILE.
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
*
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
     DISPLAY  "*** CHU0010I START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     I-O       DSPFILE.
     OPEN     INPUT     HJYOKEN.
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
*----<< ﾊﾝｲｼﾃｲ. ﾆｭｳﾘｮｸ >>-*
     PERFORM  220-INP-GRP01  UNTIL     GR-NO    NOT  =    1.
*----<< ﾊﾝｲｼﾃｲ. ﾆｭｳﾘｮｸ >>-*
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
     CLOSE    HJYOKEN.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** CHU0010I END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾃﾞｨｽﾌﾟﾚｰ  ｼｮｷ ﾋｮｳｼﾞ                         *
*--------------------------------------------------------------*
 210-DSP-INIT           SECTION.
     MOVE    "210-DSP-INIT"  TO   S-NAME.
     MOVE     SPACE          TO   FHU00101.
*
     PERFORM  CLR-HEAD-RTN.
     PERFORM  CLR-TAIL-RTN.
*
     MOVE    "CHU0010I"      TO   PGID.
     MOVE    "FHU00101"      TO   FORM.
*
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     "FHU00101"     TO   DSP-FMT.
     MOVE     "SCREEN"       TO   DSP-GRP.
     PERFORM  900-DSP-WRITE.
*
     MOVE     1              TO   GR-NO.
 210-DSP-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾝｲｼﾃｲ   ﾆｭｳﾘｮｸ                             *
*--------------------------------------------------------------*
 220-INP-GRP01          SECTION.
     MOVE     "220-INP-GRP01"     TO   S-NAME.
     MOVE     "GRP001"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      4010      TO   PROGRAM-STATUS
              MOVE      99        TO   GR-NO
         WHEN ENT
              PERFORM   CLR-HEAD-RTN
              PERFORM   220-GRP01-CHECK-SEC
              IF        ERR-FLG   =    ZERO
                        MOVE      2    TO   GR-NO
              END-IF
         WHEN OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 220-INP-GRP01-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾞｯﾁNO. ﾁｪｯｸ                                *
*--------------------------------------------------------------*
 220-GRP01-CHECK-SEC    SECTION.
     MOVE     "220-CRP01-CHECK-SEC"    TO   S-NAME.
     MOVE     ZERO      TO        ERR-FLG.
*
 220-GRP01-CHECK-01.     *>抽出区分チェック
     IF       TKBN  =  SPACE  OR  "1"  OR  "2"
**************抽出区分で日付名セット
              EVALUATE  TKBN
                  WHEN  SPACE  MOVE  NC"共通日"  TO  SENDT
                  WHEN  "1"    MOVE  NC"受信日"  TO  SENDT
                  WHEN  "2"    MOVE  NC"登録日"  TO  SENDT
                  WHEN  OTHER  MOVE  NC"＊＊＊"  TO  SENDT
              END-EVALUATE
     ELSE
              MOVE      5         TO   ERR-FLG
              MOVE     "C"        TO   EDIT-CURSOR OF TKBN
              MOVE     "R"        TO   EDIT-OPTION OF TKBN
********      GO   TO   220-GRP01-CHECK-EXIT
     END-IF.
*
 220-GRP01-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾝｲｼﾃｲ   ﾆｭｳﾘｮｸ                             *
*--------------------------------------------------------------*
 220-INP-GRP02          SECTION.
     MOVE     "220-INP-GRP02"     TO   S-NAME.
     MOVE     "GRP002"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      4010      TO   PROGRAM-STATUS
              MOVE      99        TO   GR-NO
         WHEN PF04
              MOVE      0         TO   GR-NO
         WHEN PF06
              MOVE      1         TO   GR-NO
         WHEN ENT
              PERFORM   CLR-HEAD-RTN
              PERFORM   220-GRP02-CHECK-SEC
              IF        ERR-FLG   =    ZERO
                        MOVE      9    TO   GR-NO
              END-IF
         WHEN OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 220-INP-GRP01-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾞｯﾁNO. ﾁｪｯｸ                                *
*--------------------------------------------------------------*
 220-GRP02-CHECK-SEC    SECTION.
     MOVE     "220-CRP01-CHECK-SEC"    TO   S-NAME.
     MOVE      ZERO                    TO   ERR-FLG.
 220-GRP02-CHECK-01.     *>取引先ＣＤ範囲チェック
     IF       TOKCDS  NOT  NUMERIC
              MOVE      ZERO           TO   TOKCDS
     END-IF.
     IF       TOKCDE  NOT  NUMERIC
              MOVE      99999999       TO   TOKCDE
     END-IF.
     IF       TOKCDS  >  TOKCDE
              IF        ERR-FLG   =    ZERO
                        MOVE      2    TO   ERR-FLG
              END-IF
              MOVE     "C"        TO   EDIT-CURSOR OF TOKCDS
              MOVE     "R"        TO   EDIT-OPTION OF TOKCDS
              MOVE     "R"        TO   EDIT-OPTION OF TOKCDE
*********     GO   TO   220-GRP02-CHECK-EXIT
     END-IF.
 220-GRP02-CHECK-02.     *>納品日チェック
*    納品日(開始)チェック
     IF       TDTS NOT NUMERIC
              MOVE      ZERO      TO   TDTS
     ELSE
              IF   TDTS  NOT =  ZERO
                   MOVE    "2"        TO        LINK-IN-KBN
                   MOVE     TDTS      TO        LINK-IN-YMD8
                   CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                                LINK-IN-YMD6
                                                LINK-IN-YMD8
                                                LINK-OUT-RET
                                                LINK-OUT-YMD8
                   IF       LINK-OUT-RET   NOT =    ZERO
                            IF   ERR-FLG   =    ZERO
                                 MOVE      6    TO   ERR-FLG
                            END-IF
                            MOVE  "C"   TO  EDIT-CURSOR OF TDTS
                            MOVE  "R"   TO  EDIT-OPTION OF TDTS
************                GO          TO   220-GRP02-CHECK-EXIT
                   END-IF
              END-IF
     END-IF.
*    納品日(終了)チェック
     IF       TDTE NOT NUMERIC
              MOVE      99999999  TO   TDTE
     ELSE
              IF   TDTE  NOT =  99999999
                   MOVE    "2"        TO        LINK-IN-KBN
                   MOVE     TDTE      TO        LINK-IN-YMD8
                   CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                                LINK-IN-YMD6
                                                LINK-IN-YMD8
                                                LINK-OUT-RET
                                                LINK-OUT-YMD8
                   IF       LINK-OUT-RET   NOT =    ZERO
                            IF   ERR-FLG   =    ZERO
                                 MOVE      6    TO   ERR-FLG
                            END-IF
                            MOVE  "C"   TO  EDIT-CURSOR OF TDTE
                            MOVE  "R"   TO  EDIT-OPTION OF TDTE
************                GO          TO   220-GRP02-CHECK-EXIT
                   END-IF
              END-IF
     END-IF.
*    納品日(大小)チェック
     IF  TDTS  >  TDTE
         IF   ERR-FLG   =    ZERO
              MOVE      2    TO   ERR-FLG
         END-IF
         MOVE  "C"   TO  EDIT-CURSOR OF TDTS
         MOVE  "R"   TO  EDIT-OPTION OF TDTS
         MOVE  "R"   TO  EDIT-OPTION OF TDTE
******   GO          TO   220-GRP02-CHECK-EXIT
     END-IF.
 220-GRP02-CHECK-03.     *>倉庫ＣＤ範囲チェック
     IF       SOKCDS  =  SPACE
              MOVE      SPACE          TO   SOKCDS
     END-IF.
     IF       SOKCDE  =  SPACE
              MOVE      "99"           TO   SOKCDE
     END-IF.
     IF       SOKCDS  >  SOKCDE
              IF        ERR-FLG   =    ZERO
                        MOVE      2    TO   ERR-FLG
              END-IF
              MOVE     "C"        TO   EDIT-CURSOR OF SOKCDS
              MOVE     "R"        TO   EDIT-OPTION OF SOKCDS
              MOVE     "R"        TO   EDIT-OPTION OF SOKCDE
**********    GO   TO   220-GRP02-CHECK-EXIT
     END-IF.
 220-GRP02-CHECK-04.     *>商品カテゴリ
*    商品カテゴリ１
     IF  CATEG1  NOT =  SPACE
         MOVE    10      TO   JYO-F01
         MOVE    CATEG1  TO   JYO-F02
         PERFORM  900-HJYOKEN-READ
         IF   HJYOKEN-INV-FLG  =  "INV"
              IF   ERR-FLG   =    ZERO
                   MOVE      3    TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF CATEG1
              MOVE     "R"   TO   EDIT-OPTION OF CATEG1
*********     GO             TO   220-GRP02-CHECK-EXIT
         ELSE
              IF  CATEG1 =  CATEG2  OR  CATEG3
                  IF   ERR-FLG   =    ZERO
                       MOVE      7    TO   ERR-FLG
                  END-IF
                  MOVE     "C"   TO   EDIT-CURSOR OF CATEG1
                  MOVE     "R"   TO   EDIT-OPTION OF CATEG1
                  MOVE     "R"   TO   EDIT-OPTION OF CATEG2
                  MOVE     "R"   TO   EDIT-OPTION OF CATEG3
*********         GO             TO   220-GRP02-CHECK-EXIT
              END-IF
         END-IF
      END-IF.
*    商品カテゴリ２
     IF  CATEG2  NOT =  SPACE
         MOVE    10      TO   JYO-F01
         MOVE    CATEG2  TO   JYO-F02
         PERFORM  900-HJYOKEN-READ
         IF   HJYOKEN-INV-FLG  =  "INV"
              IF   ERR-FLG   =    ZERO
                   MOVE      3    TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF CATEG2
              MOVE     "R"   TO   EDIT-OPTION OF CATEG2
***********   GO             TO   220-GRP02-CHECK-EXIT
         ELSE
              IF  CATEG2 =  CATEG1  OR  CATEG3
                  IF   ERR-FLG   =    ZERO
                       MOVE      7    TO   ERR-FLG
                  END-IF
                  MOVE     "C"   TO   EDIT-CURSOR OF CATEG2
                  MOVE     "R"   TO   EDIT-OPTION OF CATEG1
                  MOVE     "R"   TO   EDIT-OPTION OF CATEG2
                  MOVE     "R"   TO   EDIT-OPTION OF CATEG3
*********         GO             TO   220-GRP02-CHECK-EXIT
              END-IF
         END-IF
      END-IF.
*    商品カテゴリ３
     IF  CATEG3  NOT =  SPACE
         MOVE    10      TO   JYO-F01
         MOVE    CATEG3  TO   JYO-F02
         PERFORM  900-HJYOKEN-READ
         IF   HJYOKEN-INV-FLG  =  "INV"
              IF   ERR-FLG   =    ZERO
                   MOVE      3    TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF CATEG3
              MOVE     "R"   TO   EDIT-OPTION OF CATEG3
********      GO             TO   220-GRP02-CHECK-EXIT
         ELSE
              IF  CATEG3 =  CATEG1  OR  CATEG2
                  IF   ERR-FLG   =    ZERO
                       MOVE      7    TO   ERR-FLG
                  END-IF
                  MOVE     "C"   TO   EDIT-CURSOR OF CATEG3
                  MOVE     "R"   TO   EDIT-OPTION OF CATEG1
                  MOVE     "R"   TO   EDIT-OPTION OF CATEG2
                  MOVE     "R"   TO   EDIT-OPTION OF CATEG3
*********         GO             TO   220-GRP02-CHECK-EXIT
              END-IF
         END-IF
      END-IF.
 220-GRP01-CHECK-05.     *>分類区分
*    分類区分１
     IF  BUNRU1  NOT =  SPACE
         MOVE    91      TO   JYO-F01
         MOVE    BUNRU1  TO   JYO-F02
         PERFORM  900-HJYOKEN-READ
         IF   HJYOKEN-INV-FLG  =  "INV"
              IF   ERR-FLG   =    ZERO
                   MOVE      4    TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF BUNRU1
              MOVE     "R"   TO   EDIT-OPTION OF BUNRU1
*******       GO             TO   220-GRP02-CHECK-EXIT
         ELSE
              IF  BUNRU1 =  BUNRU2  OR  BUNRU3
                  IF   ERR-FLG   =    ZERO
                       MOVE      8    TO   ERR-FLG
                  END-IF
                  MOVE     "C"   TO   EDIT-CURSOR OF BUNRU1
                  MOVE     "R"   TO   EDIT-OPTION OF BUNRU1
                  MOVE     "R"   TO   EDIT-OPTION OF BUNRU2
                  MOVE     "R"   TO   EDIT-OPTION OF BUNRU3
*********         GO             TO   220-GRP02-CHECK-EXIT
              END-IF
         END-IF
      END-IF.
*    分類区分２
     IF  BUNRU2  NOT =  SPACE
         MOVE    91      TO   JYO-F01
         MOVE    BUNRU2  TO   JYO-F02
         PERFORM  900-HJYOKEN-READ
         IF   HJYOKEN-INV-FLG  =  "INV"
              IF   ERR-FLG   =    ZERO
                   MOVE      4    TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF BUNRU2
              MOVE     "R"   TO   EDIT-OPTION OF BUNRU2
*******       GO             TO   220-GRP02-CHECK-EXIT
         ELSE
              IF  BUNRU2 =  BUNRU1  OR  BUNRU3
                  IF   ERR-FLG   =    ZERO
                       MOVE      8    TO   ERR-FLG
                  END-IF
                  MOVE     "C"   TO   EDIT-CURSOR OF BUNRU2
                  MOVE     "R"   TO   EDIT-OPTION OF BUNRU1
                  MOVE     "R"   TO   EDIT-OPTION OF BUNRU2
                  MOVE     "R"   TO   EDIT-OPTION OF BUNRU3
*********         GO             TO   220-GRP02-CHECK-EXIT
              END-IF
         END-IF
      END-IF.
*    分類区分３
     IF  BUNRU3  NOT =  SPACE
         MOVE    91      TO   JYO-F01
         MOVE    BUNRU3  TO   JYO-F02
         PERFORM  900-HJYOKEN-READ
         IF   HJYOKEN-INV-FLG  =  "INV"
              IF   ERR-FLG   =    ZERO
                   MOVE      4    TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF BUNRU3
              MOVE     "R"   TO   EDIT-OPTION OF BUNRU3
******        GO             TO   220-GRP02-CHECK-EXIT
         ELSE
              IF  BUNRU3 =  BUNRU1  OR  BUNRU2
                  IF   ERR-FLG   =    ZERO
                       MOVE      8    TO   ERR-FLG
                  END-IF
                  MOVE     "C"   TO   EDIT-CURSOR OF BUNRU3
                  MOVE     "R"   TO   EDIT-OPTION OF BUNRU1
                  MOVE     "R"   TO   EDIT-OPTION OF BUNRU2
                  MOVE     "R"   TO   EDIT-OPTION OF BUNRU3
*********         GO             TO   220-GRP02-CHECK-EXIT
              END-IF
         END-IF
      END-IF.
*
 220-GRP01-CHECK-EXIT.
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
              MOVE      4010      TO   PROGRAM-STATUS
              MOVE      99        TO   GR-NO
         WHEN PF04
              MOVE      0         TO   GR-NO
         WHEN PF06
              MOVE      2         TO   GR-NO
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
     MOVE     SPACE               TO   PARA-TKBN.
     MOVE     ZERO                TO   PARA-STTOKCD.
     MOVE     ZERO                TO   PARA-EDTOKCD.
     MOVE     ZERO                TO   PARA-STDT.
     MOVE     ZERO                TO   PARA-EDDT.
     MOVE     SPACE               TO   PARA-STSOKCD.
     MOVE     SPACE               TO   PARA-EDSOKCD.
     MOVE     SPACE               TO   PARA-CATEG1.
     MOVE     SPACE               TO   PARA-CATEG2.
     MOVE     SPACE               TO   PARA-CATEG3.
     MOVE     SPACE               TO   PARA-BUNRU1.
     MOVE     SPACE               TO   PARA-BUNRU2.
     MOVE     SPACE               TO   PARA-BUNRU3.
*
     MOVE     TKBN                TO   PARA-TKBN.
     MOVE     TOKCDS              TO   PARA-STTOKCD.
     MOVE     TOKCDE              TO   PARA-EDTOKCD.
     MOVE     TDTS                TO   PARA-STDT.
     MOVE     TDTE                TO   PARA-EDDT.
     MOVE     SOKCDS              TO   PARA-STSOKCD.
     MOVE     SOKCDE              TO   PARA-EDSOKCD.
     MOVE     CATEG1              TO   PARA-CATEG1.
     MOVE     CATEG2              TO   PARA-CATEG2.
     MOVE     CATEG3              TO   PARA-CATEG3.
     MOVE     BUNRU1              TO   PARA-BUNRU1.
     MOVE     BUNRU2              TO   PARA-BUNRU2.
     MOVE     BUNRU3              TO   PARA-BUNRU3.
*
*    DISPLAY "PARA-TKBN      = " PARA-TKBN       UPON CONS.
*    DISPLAY "PARA-STTOKCD   = " PARA-STTOKCD    UPON CONS.
*    DISPLAY "PARA-EDTOKCD   = " PARA-EDTOKCD    UPON CONS.
*    DISPLAY "PARA-STDT      = " PARA-STDT     UPON CONS.
*    DISPLAY "PARA-EDDT      = " PARA-EDDT     UPON CONS.
*    DISPLAY "PARA-STSOKCD   = " PARA-STSOKCD    UPON CONS.
*    DISPLAY "PARA-EDSOKCD   = " PARA-EDSOKCD    UPON CONS.
*    DISPLAY "PARA-GATEG1    = " PARA-CATEG1     UPON CONS.
*    DISPLAY "PARA-GATEG2    = " PARA-CATEG2     UPON CONS.
*    DISPLAY "PARA-GATEG3    = " PARA-CATEG3     UPON CONS.
*    DISPLAY "PARA-BUNRU1    = " PARA-BUNRU1     UPON CONS.
*    DISPLAY "PARA-BUNRU2    = " PARA-BUNRU2     UPON CONS.
*    DISPLAY "PARA-BUNRU3    = " PARA-BUNRU3     UPON CONS.
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
              MOVE      "D"      TO   EDIT-OPTION OF MSG
     ELSE
              MOVE      MSG-TBL (ERR-FLG)   TO   MSG
     END-IF.
     IF       GR-NO     =    1
              MOVE      GUIDE01   TO   GUIDE
     ELSE
              MOVE      GUIDE02   TO   GUIDE
     END-IF.
     MOVE     WK-SYSYMD           TO   SYSYMD.
     ACCEPT   SYS-TIME2      FROM TIME.
     MOVE     SYS-TIMEW           TO   SYSTIM.
*
     PERFORM  900-DSP-WRITE.
*
     IF       MSG  NOT  =    SPACE
              MOVE "AL"      TO   DSP-PRO
     ELSE
              MOVE "NE"      TO   DSP-PRO
     END-IF.
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
     MOVE     SPACE          TO   DSP-PRO.
     WRITE    FHU00101.
 900-DSP-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＨＥＡＤ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-HEAD-RTN           SECTION.
     MOVE     " "            TO   EDIT-CURSOR OF TKBN
                                  EDIT-CURSOR OF TOKCDS
                                  EDIT-CURSOR OF TOKCDE
                                  EDIT-CURSOR OF TDTS
                                  EDIT-CURSOR OF TDTE
                                  EDIT-CURSOR OF SOKCDS
                                  EDIT-CURSOR OF SOKCDE
                                  EDIT-CURSOR OF CATEG1
                                  EDIT-CURSOR OF CATEG2
                                  EDIT-CURSOR OF CATEG3
                                  EDIT-CURSOR OF BUNRU1
                                  EDIT-CURSOR OF BUNRU2
                                  EDIT-CURSOR OF BUNRU3.
     MOVE     "M"            TO   EDIT-OPTION OF TKBN
                                  EDIT-OPTION OF TOKCDS
                                  EDIT-OPTION OF TOKCDE
                                  EDIT-OPTION OF TDTS
                                  EDIT-OPTION OF TDTE
                                  EDIT-OPTION OF SOKCDS
                                  EDIT-OPTION OF SOKCDE
                                  EDIT-OPTION OF CATEG1
                                  EDIT-OPTION OF CATEG2
                                  EDIT-OPTION OF CATEG3
                                  EDIT-OPTION OF BUNRU1
                                  EDIT-OPTION OF BUNRU2
                                  EDIT-OPTION OF BUNRU3.
 CLR-HEAD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＴＡＩＬ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-TAIL-RTN           SECTION.
     MOVE     " "        TO   EDIT-CURSOR OF KKNN.
     MOVE     "M"        TO   EDIT-OPTION OF KKNN.
 CLR-TAIL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     条件ファイル索引                            *
*--------------------------------------------------------------*
 900-HJYOKEN-READ       SECTION.
*
     READ  HJYOKEN
           INVALID      MOVE   "INV"    TO   HJYOKEN-INV-FLG
           NOT  INVALID MOVE   SPACE    TO   HJYOKEN-INV-FLG
     END-READ.
*
 900-HJYOKEN-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
