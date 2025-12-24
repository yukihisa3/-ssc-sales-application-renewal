# SBM9000I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SBM9000I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　流通ＢＭＳ　　　　　　　　　　　　*
*    モジュール名　　　　：　各種データ削除処理　　　　　　　　*
*    作成日／更新日　　　：　12/11/15                          *
*    作成者／更新者　　　：　三浦　　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*    更新日／更新者　　　：　                                  *
*    更新概要　　　　　　：　データ削除の指示を行う。　　　　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SBM9000I.
 AUTHOR.                NAV.
 DATE-WRITTEN.          12/11/15.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
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
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS    ASSIGN         DA-01-VI-TOKMS2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TOK-F01
                        STATUS         HTOKMS-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 表示ファイル >>--*
 FD  DSPFILE            LABEL     RECORD   IS   STANDARD.
     COPY     FBM90001  OF        XMDLIB.
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  HTOKMS-ST         PIC  X(02).
*----<< ﾃﾞｨｽﾌﾟﾚｲ ｺﾝﾄﾛｰﾙ ｴﾘｱ >>-*
 01  GR-NO              PIC  9(02).
 01  WK-GRP             PIC  X(08).
 01  WK-JIKAN.
     03  WK-HH          PIC   9(02)  VALUE  ZERO.
     03  WK-MM          PIC   9(02)  VALUE  ZERO.
 01  DSP-CNTL.
     03  DSP-ST         PIC  X(02).
     03  DSP-ST2        PIC  X(04).
     03  DSP-FMT        PIC  X(08).
     03  DSP-GRP        PIC  X(08).
     03  DSP-PRO        PIC  X(02).
     03  DSP-FNC        PIC  X(04).
     03  DSP-CON        PIC  X(06).
 01  MSG-AREA.
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
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
 01  WK-SYSTIME.
     03  WK-SYSHH       PIC  9(02).
     03  FILLER         PIC  X(01)     VALUE ":".
     03  WK-SYSMN       PIC  9(02).
     03  FILLER         PIC  X(01)     VALUE ":".
     03  WK-SYSSS       PIC  9(02).
*
*----<< ﾌｱﾝｸｼﾖﾝ ｷｰ ﾃ-ﾌﾞﾙ >>-*
 01  FNC-TABLE.
     03  ENT            PIC  X(04)     VALUE     "E000".
     03  PF04           PIC  X(04)     VALUE     "F004".
     03  PF05           PIC  X(04)     VALUE     "F005".
     03  PF06           PIC  X(04)     VALUE     "F006".
*
*----<< ﾌｱﾝｸｼﾖﾝ ｷｰ ｶﾞｲﾄﾞ >>-*
 01  GUIDE01       PIC  N(40)  VALUE
         NC"_取　消　_終　了".
 01  GUIDE02       PIC  N(40)  VALUE
         NC"_取　消　_終　了　_戻る".
*
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD       PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-DELNO             PIC   X(02).
 01  PARA-DELPTN            PIC   X(01).
 01  PARA-BTDATE            PIC   9(08).
 01  PARA-BTTIME            PIC   9(04).
 01  PARA-TORICD            PIC   9(08).
 01  PARA-RCVDT             PIC   9(08).
****************************************************************
 PROCEDURE              DIVISION  USING  PARA-DELNO
                                         PARA-DELPTN
                                         PARA-BTDATE
                                         PARA-BTTIME
                                         PARA-TORICD
                                         PARA-RCVDT.
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
     DISPLAY  "### SBM90001 DSPFILE ERROR " DSP-CNTL " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
**** CLOSE    HTOKMS    DSPFILE.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP     RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SBM9000I HTOKMS ERROR " HTOKMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
**** CLOSE    HTOKMS    DSPFILE.
     MOVE        "4000"    TO        PROGRAM-STATUS.
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
                                  LINK-OUT-YMD.
     IF       LINK-OUT-RET   =    ZERO
         MOVE LINK-OUT-YMD  TO   SYS-DATEW
     ELSE
         MOVE ZERO           TO   SYS-DATEW
     END-IF.
*
     MOVE     SYS-YYW        TO   WK-SYSYY.
     MOVE     SYS-MMW        TO   WK-SYSMM.
     MOVE     SYS-DDW        TO   WK-SYSDD.
*
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     SYS-HH         TO   WK-SYSHH.
     MOVE     SYS-MN         TO   WK-SYSMN.
     MOVE     SYS-SS         TO   WK-SYSSS.
*
     DISPLAY  "*** SBM9000I START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     I-O       DSPFILE.
     OPEN     INPUT     HTOKMS.
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
*----<< ﾊﾝｲ ｼﾃｲ ﾆｭｳﾘｮｸ >>-*
     PERFORM  220-INP-GRP01  UNTIL     GR-NO    NOT  =    1.
*----<< ﾊﾞｯﾁｼﾃｲ        ｱ >>-*
     PERFORM  220-INP-GRP02  UNTIL     GR-NO    NOT  =    2.
*----<< ｿｳｼﾞｭｼﾝﾋﾞｼﾃｲ >>-*
     PERFORM  220-INP-GRP03  UNTIL     GR-NO    NOT  =    3.
*----<< ｶｸﾆﾝ ﾆｭｳﾘｮｸ >>-*
     PERFORM  230-INP-KKNN   UNTIL     GR-NO    NOT  =    9.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     MOVE    "300-END-RTN"        TO   S-NAME.
     CLOSE    DSPFILE.
     CLOSE    HTOKMS.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SBM9000I END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾃﾞｨｽﾌﾟﾚｰ  ｼｮｷ ﾋｮｳｼﾞ                         *
*--------------------------------------------------------------*
 210-DSP-INIT           SECTION.
     MOVE    "210-DSP-INIT"       TO   S-NAME.
     MOVE     SPACE          TO   FBM90001.
*
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     "FBM90001"     TO   DSP-FMT.
     MOVE     "SCREEN"       TO   DSP-GRP.
     PERFORM  900-DSP-WRITE.
*
     PERFORM  215-DSP-SYOKI.
*
     MOVE     1              TO   GR-NO.
 210-DSP-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*             画面制御項目初期化                               *
*--------------------------------------------------------------*
 215-DSP-SYOKI         SECTION.
     MOVE     "215-DSP-SYOKI"      TO   S-NAME.
*
*リバース，カーソルパーク解除
***  指定種類
     MOVE    "M"      TO  EDIT-OPTION  OF  DELPTN.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DELPTN.
***  バッチ日付
     MOVE    "M"      TO  EDIT-OPTION  OF  BTDATE.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  BTDATE.
***  バッチ時間
     MOVE    "M"      TO  EDIT-OPTION  OF  BTTIME.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  BTTIME.
***  取引先
     MOVE    "M"      TO  EDIT-OPTION  OF  TORICD.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  TORICD.
***  開始日
     MOVE    "M"      TO  EDIT-OPTION  OF  RCVDT.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  RCVDT.
***  データ名
     EVALUATE PARA-DELNO
         WHEN "01"
              MOVE  NC"発注メッセージ"   TO   DELDT
         WHEN "02"
              MOVE  NC"出荷メッセージ"   TO   DELDT
         WHEN "03"
              MOVE  NC"受領メッセージ"   TO   DELDT
         WHEN "04"
              MOVE  NC"受領訂正メッセージ"   TO   DELDT
         WHEN "05"
              MOVE  NC"返品メッセージ"   TO   DELDT
         WHEN "06"
              MOVE  NC"支払メッセージ"   TO   DELDT
         WHEN "07"
              MOVE  NC"請求メッセージ"   TO   DELDT
         WHEN OTHER
              MOVE NC"　"   TO   DELDT
     END-EVALUATE.
 215-DSP-SYOKI-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾝｲ      ﾆｭｳﾘｮｸ                             *
*--------------------------------------------------------------*
 220-INP-GRP01          SECTION.
     MOVE     "220-INP-GRP01"     TO   S-NAME.
     MOVE     "PARGRP"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
              MOVE    "4010"   TO   PROGRAM-STATUS
         WHEN PF04
              MOVE      0         TO   GR-NO
         WHEN ENT
              PERFORM   225-PARA1-CHK
         WHEN OTHER
              MOVE NC"ＰＦキーが違います"   TO   MSGSPC
     END-EVALUATE.
*
 220-INP-GRP01-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾞｯﾁｼﾃｲ  ﾆｭｳﾘｮｸ                             *
*--------------------------------------------------------------*
 220-INP-GRP02          SECTION.
     MOVE     "220-INP-GRP02"     TO   S-NAME.
     MOVE     "BTGRP"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
              MOVE    "4010"   TO   PROGRAM-STATUS
         WHEN PF04
              MOVE      0         TO   GR-NO
         WHEN PF06
              MOVE      1         TO   GR-NO
              MOVE    "M"      TO  EDIT-OPTION  OF  BTDATE
              MOVE    SPACE    TO  EDIT-CURSOR  OF  BTDATE
              MOVE    SPACE    TO  BTDATE(1:8)
              MOVE    "M"      TO  EDIT-OPTION  OF  BTTIME
              MOVE    SPACE    TO  EDIT-CURSOR  OF  BTTIME
              MOVE    SPACE    TO  BTTIME(1:4)
              MOVE    "M"      TO  EDIT-OPTION  OF  TORICD
              MOVE    SPACE    TO  EDIT-CURSOR  OF  TORICD
              MOVE    SPACE    TO  TORICD(1:8)  TORINM
         WHEN ENT
              PERFORM   225-PARA2-CHK
         WHEN OTHER
              MOVE NC"ＰＦキーが違います"   TO   MSGSPC
     END-EVALUATE.
*
 220-INP-GRP02-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾞｯﾁｼﾃｲ  ﾆｭｳﾘｮｸ                             *
*--------------------------------------------------------------*
 220-INP-GRP03          SECTION.
     MOVE     "220-INP-GRP03"     TO   S-NAME.
     MOVE     "DTGRP"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
              MOVE    "4010"   TO   PROGRAM-STATUS
         WHEN PF04
              MOVE      0         TO   GR-NO
         WHEN PF06
              MOVE      1         TO   GR-NO
              MOVE    "M"      TO  EDIT-OPTION  OF  RCVDT
              MOVE    SPACE    TO  EDIT-CURSOR  OF  RCVDT
              MOVE    SPACE    TO  RCVDT(1:8)
         WHEN ENT
              PERFORM   225-PARA3-CHK
         WHEN OTHER
              MOVE NC"ＰＦキーが違います"   TO   MSGSPC
     END-EVALUATE.
*
 220-INP-GRP02-EXIT.
     EXIT.
*--------------------------------------------------------------*
*             パラメタチェック１                               *
*--------------------------------------------------------------*
 225-PARA1-CHK                 SECTION.
     MOVE     "PARA-CHK1"     TO   S-NAME.
*
*    指定種類チェック
     IF    DELPTN    =   "1"
     OR    DELPTN    =   "2"
            MOVE    "M"    TO   EDIT-OPTION  OF  DELPTN
            MOVE     SPACE TO   EDIT-CURSOR  OF  DELPTN
     ELSE
            MOVE NC"指定種類には１か２を入力して下さい。"
            TO  MSGSPC
            MOVE     "C"   TO   EDIT-CURSOR OF DELPTN
            MOVE     "R"   TO   EDIT-OPTION OF DELPTN
            GO             TO   PARA-CHK1-EXIT
     END-IF.
     IF    DELPTN    =   "1"
           MOVE     2        TO    GR-NO
     ELSE
           MOVE     3        TO    GR-NO
     END-IF.
*
 PARA-CHK1-EXIT.
     EXIT.
*--------------------------------------------------------------*
*             パラメタチェック２                               *
*--------------------------------------------------------------*
 225-PARA2-CHK                 SECTION.
     MOVE     "PARA-CHK2"     TO   S-NAME.
*
*    （バッチ日付）
     IF       BTDATE   NOT NUMERIC
     OR       BTDATE   =     ZERO
                   MOVE  NC"バッチ日付を入力して下さい"
                                  TO   MSGSPC
              MOVE  "R"      TO   EDIT-OPTION  OF  BTDATE
              MOVE  "C"      TO   EDIT-CURSOR  OF  BTDATE
              GO             TO   PARA-CHK2-EXIT
     ELSE
              MOVE    "M"    TO   EDIT-OPTION  OF  BTDATE
              MOVE     SPACE TO   EDIT-CURSOR  OF  BTDATE
     END-IF.
*    論理チェック
     IF       RCVDT    NOT =  ZERO
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     BTDATE         TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   MOVE  NC"バッチ日付を正しく入力して下さい"
                                  TO   MSGSPC
                   MOVE  "R"      TO   EDIT-OPTION  OF  BTDATE
                   MOVE  "C"      TO   EDIT-CURSOR  OF  BTDATE
                   GO             TO   PARA-CHK2-EXIT
              ELSE
                   MOVE  "M"      TO   EDIT-OPTION  OF  BTDATE
                   MOVE   SPACE   TO   EDIT-CURSOR  OF  BTDATE
              END-IF
     END-IF.
*取引先チェック
*バッチ時間チェック
***  バッチ時間未入力チェック
     IF       BTTIME  NOT NUMERIC
***      OR   BTTIME  =   ZERO
                   MOVE NC"バッチ_を入力して下さい"  TO  MSGSPC
                   MOVE  "R"      TO   EDIT-OPTION  OF  BTTIME
                   MOVE  "C"      TO   EDIT-CURSOR  OF  BTTIME
                   GO             TO   PARA-CHK2-EXIT
     ELSE
***           バッチ（時間）論理チェック
              MOVE     BTTIME    TO  WK-JIKAN
              IF       WK-HH  >  23 OR
                       WK-MM  >  59
                       MOVE   NC"バッチ_論理エラー"   TO MSGSPC
                       MOVE "R"    TO EDIT-OPTION OF BTTIME
                       MOVE "C"    TO EDIT-CURSOR OF BTTIME
                       GO          TO PARA-CHK2-EXIT
              ELSE
                       MOVE "M"    TO EDIT-OPTION OF BTTIME
                       MOVE SPACE  TO EDIT-CURSOR OF BTTIME
              END-IF
     END-IF.
***  取引先未入力チェック
     IF       TORICD  NOT NUMERIC
         OR   TORICD  =  ZERO
              MOVE NC"取引先を入力して下さい"    TO  MSGSPC
              MOVE  "R"      TO   EDIT-OPTION  OF  TORICD
              MOVE  "C"      TO   EDIT-CURSOR  OF  TORICD
              GO             TO   PARA-CHK2-EXIT
     ELSE
***           取引先ＲＥＡＤ
              MOVE     TORICD TO   TOK-F01
              READ     HTOKMS
              INVALID
                     MOVE
               NC"取引先マスタに登録されていません" TO MSGSPC
                     MOVE  "R"    TO   EDIT-OPTION  OF  TORICD
                     MOVE  "C"    TO   EDIT-CURSOR  OF  TORICD
                     MOVE   SPACE TO   TORINM
                     GO           TO   PARA-CHK2-EXIT
              NOT INVALID
                     MOVE  "M"    TO   EDIT-OPTION  OF  TORICD
                     MOVE  SPACE  TO   EDIT-CURSOR  OF  TORICD
                     MOVE  TOK-F03  TO TORINM
              END-READ
     END-IF.
*
*
     MOVE     9        TO    GR-NO.
*
 PARA-CHK2-EXIT.
     EXIT.
*--------------------------------------------------------------*
*             パラメタチェック３                               *
*--------------------------------------------------------------*
 225-PARA3-CHK                 SECTION.
     MOVE     "PARA-CHK3"     TO   S-NAME.
*
*    （送受信日）
     IF       RCVDT    NOT NUMERIC
     OR       RCVDT    =     ZERO
                   MOVE  NC"送受信日を入力して下さい"
                                  TO   MSGSPC
              MOVE  "R"      TO   EDIT-OPTION  OF  RCVDT
              MOVE  "C"      TO   EDIT-CURSOR  OF  RCVDT
              GO             TO   PARA-CHK3-EXIT
     ELSE
              MOVE    "M"    TO   EDIT-OPTION  OF  RCVDT
              MOVE     SPACE TO   EDIT-CURSOR  OF  RCVDT
     END-IF.
*    論理チェック
     IF       RCVDT    NOT =  ZERO
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     RCVDT          TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   MOVE  NC"送受信日を正しく入力して下さい"
                                  TO   MSGSPC
                   MOVE  "R"      TO   EDIT-OPTION  OF  RCVDT
                   MOVE  "C"      TO   EDIT-CURSOR  OF  RCVDT
                   GO             TO   PARA-CHK3-EXIT
              ELSE
                   MOVE  "M"      TO   EDIT-OPTION  OF  RCVDT
                   MOVE   SPACE   TO   EDIT-CURSOR  OF  RCVDT
              END-IF
     END-IF.
*
     MOVE     9        TO    GR-NO.
*
 PARA-CHK3-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｶｸﾆﾝ ﾆｭｳﾘｮｸ                                 *
*--------------------------------------------------------------*
 230-INP-KKNN           SECTION.
     MOVE     "230-INP-KKNN"      TO   S-NAME.
     MOVE     "CHKGRP"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
              MOVE    "4010"   TO   PROGRAM-STATUS
         WHEN PF04
              MOVE      0         TO   GR-NO
         WHEN PF06
              MOVE      1         TO   GR-NO
              MOVE    "M"      TO  EDIT-OPTION  OF  BTDATE
              MOVE    SPACE    TO  EDIT-CURSOR  OF  BTDATE
              MOVE    SPACE    TO  BTDATE(1:8)
              MOVE    "M"      TO  EDIT-OPTION  OF  BTTIME
              MOVE    SPACE    TO  EDIT-CURSOR  OF  BTTIME
              MOVE    SPACE    TO  BTTIME(1:4)
              MOVE    "M"      TO  EDIT-OPTION  OF  TORICD
              MOVE    SPACE    TO  EDIT-CURSOR  OF  TORICD
              MOVE    SPACE    TO  TORICD(1:8)  TORINM
              MOVE    "M"      TO  EDIT-OPTION  OF  RCVDT
              MOVE    SPACE    TO  EDIT-CURSOR  OF  RCVDT
              MOVE    SPACE    TO  RCVDT(1:8)
         WHEN ENT
              MOVE      99        TO   GR-NO
              MOVE      DELPTN    TO   PARA-DELPTN
              MOVE      BTDATE    TO   PARA-BTDATE
              MOVE      BTTIME    TO   PARA-BTTIME
              MOVE      TORICD    TO   PARA-TORICD
              MOVE      RCVDT     TO   PARA-RCVDT
         WHEN OTHER
              MOVE NC"ＰＦキーが違います"   TO   MSGSPC
     END-EVALUATE.
*
 230-INP-KKNN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  READ                              *
*--------------------------------------------------------------*
 900-DSP-READ           SECTION.
     MOVE     "900-DSP-READ"      TO   S-NAME.
     MOVE     "SCREEN"       TO   DSP-GRP.
     IF       GR-NO     =    1
              MOVE      GUIDE01   TO   FNCSPC
     ELSE
              MOVE      GUIDE02   TO   FNCSPC
     END-IF.
     MOVE     WK-SYSYMD           TO   SDATE.
     MOVE     WK-SYSTIME          TO   STIME.
*
     PERFORM  900-DSP-WRITE.
*
     IF       MSGSPC  NOT  =    SPACE
              MOVE "AL"      TO   DSP-PRO
     ELSE
              MOVE "NE"      TO   DSP-PRO
     END-IF.
     MOVE     SPACE          TO   MSGSPC.
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
     WRITE    FBM90001.
 900-DSP-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    取引先マスタ　 READ                          *
*--------------------------------------------------------------*
 900-TOK-READ           SECTION.
     MOVE     "900-TOK-READ"      TO   S-NAME.
     READ     HTOKMS    INVALID
              MOVE      SPACE          TO   TOK-F03
     END-READ.
 900-TOK-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
