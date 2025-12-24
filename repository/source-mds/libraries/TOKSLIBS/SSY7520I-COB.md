# SSY7520I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY7520I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ　　　　　　　　*
*    業務名　　　　　　　：　ＤＣＭサンワ　　　　　　　　　　　*
*    モジュール名　　　　：　支払明細書発行指示　　　　　　　　*
*    作成日／更新日　　　：　2015/11/26                        *
*    作成者／更新者　　　：　ＮＡＶ宮下　　　　　　　　　　　　*
*    ＯＵＴＰＵＴ　　　　：　出力区分     PARA-OUTPTN          *
*                        ：　取引先コード PARA-TORICD          *
*                        ：　支払締年月　 PARA-NENGETU         *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*    更新日／更新者　　　：　                                  *
*    更新概要　　　　　　：　支払明細書の発行指示を行う。　　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY7520I.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2015/11/26.
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
*----<< 支払明細データ >>--*
     SELECT   SWSIHAL1  ASSIGN         DA-01-VI-SWSIHAL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SIH-F01
                                       SIH-F02
                                       SIH-F03
                                       SIH-F06
                                       SIH-F04
                        STATUS         SWSIHAL1-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 表示ファイル >>--*
 FD  DSPFILE            LABEL     RECORD   IS   STANDARD.
     COPY     FSY75201  OF        XMDLIB.
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL     RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< 支払明細データ >>--*
 FD  SWSIHAL1            LABEL     RECORD   IS   STANDARD.
     COPY     SWSIHAF   OF        XFDLIB
              JOINING   SIH       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  HTOKMS-ST         PIC  X(02).
 01  SWSIHAL1-ST       PIC  X(02).
*
*----<< INVALID FLG >>--*
 01  TOK-INVALID-FLG    PIC  X(01).
 01  SIH-INVALID-FLG    PIC  X(01).
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
 01  MSG-AREA.
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
*
 01  WK-NENGETU.
     03  WK-YYYY            PIC   9(04).
     03  WK-MM              PIC   9(02).
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
         NC"Ｆ４：取消　Ｆ５：終了".
 01  GUIDE02       PIC  N(40)  VALUE
         NC"Ｆ４：取消　Ｆ５：終了　Ｆ６：戻る".
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
 01  PARA-OUTPTN            PIC   X(01).
 01  PARA-TORICD            PIC   9(08).
 01  PARA-NENGETU           PIC   9(06).
****************************************************************
 PROCEDURE              DIVISION  USING  PARA-OUTPTN
                                         PARA-TORICD
                                         PARA-NENGETU.
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
     DISPLAY  "### SSY7520I DSPFILE ERROR " DSP-CNTL " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP     RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY7520I HTOKMS ERROR " HTOKMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP     RUN.
*----<< 受領ＭＳＧＬ１ >>--*
 SWSIHAL1-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SWSIHAL1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY7520I SWJYURF ERROR " SWSIHAL1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP     RUN.
*
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
     DISPLAY  "*** SSY7520I START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*----<<FILE OPEN >>--*
     OPEN     I-O       DSPFILE.
     OPEN     INPUT     HTOKMS.
     OPEN     INPUT     SWSIHAL1.
*
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
*----<< ｶｸﾆﾝ ﾆｭｳﾘｮｸ >>-*
     PERFORM  230-INP-KKNN   UNTIL     GR-NO    NOT  =    9.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     MOVE    "300-END-RTN"        TO   S-NAME.
*
*----<<FILE CLOSE >>--*
     CLOSE    DSPFILE.
     CLOSE    HTOKMS.
     CLOSE    SWSIHAL1.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSY7520I END *** "
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
     MOVE     SPACE          TO   FSY75201.
*
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     "FSY75201"     TO   DSP-FMT.
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
***  出力種類
     MOVE    "M"      TO  EDIT-OPTION  OF  OUTPTN.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  OUTPTN.
***  取引先
     MOVE    "M"      TO  EDIT-OPTION  OF  TORICD.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  TORICD.
***  支払締年月
     MOVE    "M"      TO  EDIT-OPTION  OF  SIHDT.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  SIHDT.
*
 215-DSP-SYOKI-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾝｲ      ﾆｭｳﾘｮｸ                             *
*--------------------------------------------------------------*
 220-INP-GRP01          SECTION.
     MOVE     "220-INP-GRP02"     TO   S-NAME.
     MOVE     "PARGRP"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
              MOVE      "4010"    TO   PROGRAM-STATUS
         WHEN PF04
              MOVE      0         TO   GR-NO
         WHEN ENT
              PERFORM   225-PARA-CHK

         WHEN OTHER
              MOVE NC"ＰＦキーが違います"   TO   MSGSPC
     END-EVALUATE.
*
 220-INP-GRP01-EXIT.
     EXIT.
*--------------------------------------------------------------*
*             画面エラーチェック                               *
*--------------------------------------------------------------*
 225-PARA-CHK                 SECTION.
     MOVE     "PARA-CHK"     TO   S-NAME.
*
*    出力種類チェック
     IF    OUTPTN    =   "1"
     OR    OUTPTN    =   "2"
            MOVE    "M"    TO   EDIT-OPTION  OF  OUTPTN
            MOVE     SPACE TO   EDIT-CURSOR  OF  OUTPTN
     ELSE
            MOVE NC"出力種類には１か２を入力して下さい。"
            TO  MSGSPC
            MOVE     "C"   TO   EDIT-CURSOR OF OUTPTN
            MOVE     "R"   TO   EDIT-OPTION OF OUTPTN
            GO             TO   PARA-CHK-EXIT
     END-IF.
*
*取引先チェック
***  取引先未入力チェック
     IF       TORICD  NOT NUMERIC
         OR   TORICD  =  ZERO
              MOVE NC"取引先を入力して下さい"    TO  MSGSPC
              MOVE  "R"      TO   EDIT-OPTION  OF  TORICD
              MOVE  "C"      TO   EDIT-CURSOR  OF  TORICD
              GO             TO   PARA-CHK-EXIT
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
                     GO           TO   PARA-CHK-EXIT
              NOT INVALID
                     MOVE  "M"    TO   EDIT-OPTION  OF  TORICD
                     MOVE  SPACE  TO   EDIT-CURSOR  OF  TORICD
                     MOVE  TOK-F03  TO TORINM
              END-READ
     END-IF.
*
*支払締年月
     IF       SIHDT   NOT NUMERIC
         OR   SIHDT   =  ZERO
              MOVE NC"支払締日を入力して下さい"    TO  MSGSPC
              MOVE  "R"      TO   EDIT-OPTION  OF  SIHDT
              MOVE  "C"      TO   EDIT-CURSOR  OF  SIHDT
              GO             TO   PARA-CHK-EXIT
     ELSE
              MOVE   SIHDT   TO   WK-NENGETU
              IF  WK-MM  >=  1
              AND WK-MM  <=  12
                     MOVE  "M"    TO   EDIT-OPTION  OF  SIHDT
                     MOVE  SPACE  TO   EDIT-CURSOR  OF  SIHDT
              ELSE
                    MOVE NC"支払締日論理エラーです" TO MSGSPC
                     MOVE  "R"    TO   EDIT-OPTION  OF  SIHDT
                     MOVE  "C"    TO   EDIT-CURSOR  OF  SIHDT
                     GO           TO   PARA-CHK-EXIT
              END-IF
              IF  WK-YYYY   >  ZERO
                  CONTINUE
              ELSE
                     MOVE  "M"    TO   EDIT-OPTION  OF  SIHDT
                     MOVE  SPACE  TO   EDIT-CURSOR  OF  SIHDT
                    MOVE NC"支払締日論理エラーです" TO MSGSPC
                     MOVE  "R"    TO   EDIT-OPTION  OF  SIHDT
                     MOVE  "C"    TO   EDIT-CURSOR  OF  SIHDT
                     GO           TO   PARA-CHK-EXIT
              END-IF
     END-IF.
*
*ＤＣＭサンワ支払明細データ存在チェック
     PERFORM  900-SIH-READ.
     IF       SIH-INVALID-FLG = "1"
              MOVE NC"支払明細ＤＴが存在しません" TO MSGSPC
              MOVE  "R"    TO   EDIT-OPTION  OF  TORICD
              MOVE  "R"    TO   EDIT-OPTION  OF  SIHDT
              MOVE  "C"    TO   EDIT-CURSOR  OF  TORICD
              GO           TO   PARA-CHK-EXIT
     END-IF.
*
     MOVE     9        TO    GR-NO.
*
 PARA-CHK-EXIT.
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
         WHEN ENT
              MOVE      99        TO   GR-NO
              MOVE      OUTPTN    TO   PARA-OUTPTN
              MOVE      TORICD    TO   PARA-TORICD
              MOVE      SIHDT     TO   PARA-NENGETU
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
     WRITE    FSY75201.
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
*--------------------------------------------------------------*
*    LEVEL ALL    支払明細データ   存在チェック                    *
*--------------------------------------------------------------*
 900-SIH-READ           SECTION.
*
*----<< 処理名セット >>--*
     MOVE     "900-SIH-READ"      TO   S-NAME.
*
*----<< クリア >>--*
     MOVE     SPACE     TO        SIH-INVALID-FLG.
     MOVE     SPACE     TO        SIH-REC.
     INITIALIZE                   SIH-REC.
*
*----<< キー情報セット >>--*
     MOVE     TORICD              TO   SIH-F02.
     MOVE     SIHDT               TO   SIH-F01.
*
*----<< FILE START >>--*
     START    SWSIHAL1  KEY  >=   SIH-F01  SIH-F02  SIH-F03
                                  SIH-F06  SIH-F04
        INVALID   KEY
           MOVE   "1"     TO   SIH-INVALID-FLG
             GO   TO   900-SIH-READ-EXIT
     END-START.
*
*----<< 読込 >>--*
     READ     SWSIHAL1
       AT END
           MOVE   "1"     TO   SIH-INVALID-FLG
           GO             TO   900-SIH-READ-EXIT
       NOT AT END
           MOVE   SPACE   TO   SIH-INVALID-FLG
     END-READ.
*
*----<< 対象チェック（支払締年月） >>--*
     IF    SIH-F01  NOT  =   SIHDT
           MOVE   "1"     TO   SIH-INVALID-FLG
           GO             TO   900-SIH-READ-EXIT
     END-IF.
*----<< 対象チェック（取引先ＣＤ） >>--*
     IF    SIH-F02  NOT  =   TORICD
           MOVE   "1"     TO   SIH-INVALID-FLG
     END-IF.
*
 900-SIH-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
