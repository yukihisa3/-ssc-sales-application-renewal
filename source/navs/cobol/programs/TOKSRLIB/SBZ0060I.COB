****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　部門間在庫移動機能構築　　　      *
*    モジュール名　　　　：　部門間移動ＤＴ入力　　　　　　　  *
*    作成日／更新日　　　：　2018/01/23                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　画面より部門間移動データの修正・　*
*                        ：　削除を行なう。既にＡＣＯＳ計上済  *
*                        ：　の場合は修正、削除不可。　　　　  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SBZ0060I.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2018/01/23.
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
*----<< 画面ファイル >>----*
     SELECT   DSPFILE   ASSIGN         01-GS-DSPF
                        FORMAT         DSP-FMT
                        GROUP          DSP-GRP
                        PROCESSING     DSP-PRO
                        UNIT CONTROL   DSP-CON
                        FUNCTION       DSP-FNC
                        STATUS         DSP-ST.
*----<< 振替移動実績ファイル >>----*
     SELECT   FURIDOL1  ASSIGN         DA-01-VI-FURIDOL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE DYNAMIC
                        RECORD    KEY  FUR-F01   FUR-F02
                                       FUR-F07
                                       WITH DUPLICATES
                        STATUS         FURIDOL1-ST.
*----<<商品名称マスタ >>----*
     SELECT   MEIMS1    ASSIGN              DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F011
                                            MEI-F012
                        FILE STATUS         MEIMS1-ST.
*----<< 倉庫マスタ >>----*
     SELECT   ZSOKMS1   ASSIGN              DA-01-VI-ZSOKMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SOK-F01
                        STATUS              ZSOKMS1-ST.
*----<< 担当者マスタ >>----*
     SELECT   TANMS1    ASSIGN    TO        DA-01-VI-TANMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TAN-F01 TAN-F02
                        FILE      STATUS    TANMS1-ST.
*----<< 条件ファイル >>-*
     SELECT   JYOKEN1   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYO-F01   JYO-F02
                        FILE      STATUS    JYOKEN1-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 画面ファイル >>--*
 FD  DSPFILE            LABEL     RECORD   IS   STANDARD.
     COPY     FBZ00601  OF        XMDLIB.
*----<< 振替移動実績ファイル >>----*
 FD  FURIDOL1           LABEL     RECORD   IS   STANDARD.
     COPY     FURIDOF   OF        XFDLIB
              JOINING   FUR       PREFIX.
*----<< 商品名称マスタ >>--*
 FD  MEIMS1             LABEL RECORD   IS   STANDARD.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
*----<< 倉庫マスタ >>--*
 FD  ZSOKMS1            LABEL RECORD   IS   STANDARD.
     COPY     ZSOKMS1   OF        XFDLIB
              JOINING   SOK       PREFIX.
*---<<  担当者マスタ  >>---*
 FD  TANMS1.
     COPY     TANMS1    OF        XFDLIB
              JOINING   TAN       PREFIX.
*----<< 条件ファイル >>--*
 FD  JYOKEN1            LABEL RECORD   IS   STANDARD.
     COPY     JYOKEN1   OF        XFDLIB
              JOINING   JYO       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  ERR-FLG             PIC  9(02)  VALUE  ZERO.
     03  KEP-FLG             PIC  X(01)  VALUE  SPACE.
     03  TANMS1-INV-FLG      PIC  X(03)  VALUE  SPACE.
     03  MEIMS1-INV-FLG      PIC  X(03)  VALUE  SPACE.
     03  ZSOKMS1-INV-FLG     PIC  X(03)  VALUE  SPACE.
     03  JYOKEN1-INV-FLG     PIC  X(03)  VALUE  SPACE.
     03  CHK-FLG             PIC  X(03)  VALUE  SPACE.
     03  ERK-FLG             PIC  9(01)  VALUE  ZERO.
     03  WK-NOUHIN           PIC  9(08)  VALUE  ZERO.
     03  WK-START-FLG        PIC  X(03)  VALUE  SPACE.
     03  WK-KAKUN-FLG        PIC  X(01)  VALUE  SPACE.
     03  WK-REC-ERR          PIC  X(01)  VALUE  SPACE.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
*01  DSP-ST            PIC  X(02).
 01  FURIDOL1-ST       PIC  X(02).
 01  MEIMS1-ST         PIC  X(02).
 01  ZSOKMS1-ST        PIC  X(02).
 01  TANMS1-ST         PIC  X(02).
 01  JYOKEN1-ST        PIC  X(02).
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
 01  WK-SYS-DATE             PIC  9(08).
 01  FILLER                  REDEFINES   WK-SYS-DATE.
     03  WK-SYS-YY           PIC  9(04).
     03  WK-SYS-MM           PIC  9(02).
     03  WK-SYS-DD           PIC  9(02).
*
 01  HENKAN-TI               PIC  9(04).
 01  GETUDO                  PIC  9(02).
 01  GETUDO2                 PIC S9(02).
 01  GETUDO2-YMD.
     03  GETUDO2-YY          PIC  9(04).
     03  GETUDO2-MM          PIC  9(02).
     03  GETUDO2-DD          PIC  9(02).
 01  GETUDO2-YMDR      REDEFINES  GETUDO2-YMD.
     03  GETUDO2-YYMMR       PIC  9(06).
     03  FILLER              PIC  X(02).
 01  WK-DEN112               PIC  9(08).
 01  FILLER                  REDEFINES      WK-DEN112.
     03  WK-DEN112YM         PIC  9(06).
     03  WK-DEN112D          PIC  9(02).
 01  WK-DEN113               PIC  9(08).
 01  FILLER                  REDEFINES      WK-DEN113.
     03  WK-DEN113YM         PIC  9(06).
     03  WK-DEN113D          PIC  9(02).
 01  ACOS-DATE               PIC  9(08).
 01  FILLER                  REDEFINES      ACOS-DATE.
     03  ACOS-YYMM           PIC  9(06).
     03  ACOS-DD             PIC  9(02).
 01  START-YYMM              PIC  9(06).
 01  FILLER                  REDEFINES      START-YYMM.
     03  START-YY            PIC  9(04).
     03  START-MM            PIC  9(02).
 01  END-YYMM                PIC  9(06).
 01  FILLER                  REDEFINES      END-YYMM.
     03  END-YY              PIC  9(04).
     03  END-MM              PIC  9(02).
 01  CHK-DATE                PIC  9(08).
 01  FILLER                  REDEFINES      CHK-DATE.
     03  CHK-YYMM            PIC  9(06).
     03  FILLER              REDEFINES      CHK-YYMM.
         05  CHK-YY          PIC  9(04).
         05  CHK-MM          PIC  9(02).
     03  CHK-DD              PIC  9(02).
 01  SAV-NOU-DATE2           PIC  9(08)    VALUE ZERO.
 01  SAV-NOU-DATE.
     03  SAV-NOU-YYMM        PIC  9(06).
     03  SAV-NOU-DD          PIC  9(02).
 01  SAV-HNOU-DATE2          PIC  9(08)    VALUE ZERO.
 01  SAV-HNOU-DATE.
     03  SAV-HNOU-YYMM       PIC  9(06).
     03  SAV-HNOU-DD         PIC  9(02).
 01  SAV-HTENCD              PIC  9(05)    VALUE ZERO.
 01  SAV-CYU-DATE            PIC  9(08)    VALUE ZERO.
 01  SAV-SYU-DATE            PIC  9(08)    VALUE ZERO.
 01  SAV-ZENGETU             PIC  9(08).
 01  SAV-ACOS-YYMM.
     03  SAV-ACOS-YY         PIC  9(04).
     03  SAV-ACOS-MM         PIC  9(02).
 01  SAV-ACOS-MMS            PIC  9(02).
***
 01  CHK-DATE-WORK.
     03  CHK-01              PIC  9(04).
     03  CHK-02              PIC  9(02).
     03  MATUBI              PIC  X(24)  VALUE
         "312831303130313130313031".
     03  FILLER              REDEFINES   MATUBI.
         05  WK-MATUBI       PIC  9(02)  OCCURS  12.
*
 01  INDEXES.
     03  I                   PIC  9(02).
     03  J                   PIC  9(02).
     03  K                   PIC  9(02).
     03  L                   PIC  9(02).
     03  T                   PIC  9(02).
     03  IL                  PIC  9(02).
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
     03  PF11           PIC  X(04)     VALUE     "F011".
     03  PF12           PIC  X(04)     VALUE     "F012".
*
*----<< ﾌｱﾝｸｼﾖﾝ ｷｰ ｶﾞｲﾄﾞ >>-*
 01  GUIDE00       PIC  N(40)  VALUE   NC"_終　了".
 01  GUIDE01       PIC  N(40)  VALUE   NC"_取　消　_終　了".
 01  GUIDE02       PIC  N(40)  VALUE
     NC"_取　消　_終　了　_項目戻り".
*
 01  MSG-AREA.
     03  MSG01               PIC  N(30)  VALUE
              NC"ＰＦキーが違います".
     03  MSG02               PIC  N(30)  VALUE
              NC"処理モードエラー（２：修正、３：削除）".
     03  MSG03               PIC  N(30)  VALUE
              NC"伝票番号を入力して下さい。".
     03  MSG04               PIC  N(30)  VALUE
              NC"店舗ＣＤを入力して下さい".
     03  MSG05               PIC  N(30)  VALUE
              NC"計上日を入力して下さい。".
     03  MSG06               PIC  N(30)  VALUE
              NC"日付論理エラー！！".
     03  MSG07               PIC  N(30)  VALUE
              NC"計上日がＡＣＯＳ締日以前です".
     03  MSG08               PIC  N(30)  VALUE
              NC"既に計上済の伝票です".
     03  MSG09               PIC  N(30)  VALUE
              NC"変更計上日がＡＣＯＳ締日以前です".
     03  MSG10               PIC  N(30)  VALUE
              NC"商品名称マスタに存在しません".
     03  MSG11               PIC  N(30)  VALUE
              NC"対象データが存在しません。".
     03  MSG12               PIC  N(30)  VALUE
              NC"変更計上日を入力して下さい。".
     03  MSG13               PIC  N(30)  VALUE
              NC"指定可能日付範囲外です。".
     03  MSG14               PIC  N(30)  VALUE
              NC"　".
     03  MSG15               PIC  N(30)  VALUE
              NC"　".
     03  MSG16               PIC  N(30)  VALUE
              NC"　".
     03  MSG17               PIC  N(30)  VALUE
              NC"　".
     03  MSG18               PIC  N(30)  VALUE
              NC"　".
     03  MSG19               PIC  N(30)  VALUE
              NC"　".
     03  MSG20               PIC  N(30)  VALUE
              NC"　".
     03  MSG21               PIC  N(30)  VALUE
              NC"　".
     03  MSG22               PIC  N(30)  VALUE
              NC"　".
     03  MSG23               PIC  N(30)  VALUE
              NC"　".
     03  MSG24               PIC  N(30)  VALUE
              NC"　".
     03  MSG25               PIC  N(30)  VALUE
              NC"　".
     03  MSG26               PIC  N(30)  VALUE
              NC"　".
     03  MSG27               PIC  N(30)  VALUE
              NC"　".
*
 01  FILLER                  REDEFINES   MSG-AREA.
     03  MSG-TBL             PIC  N(30)  OCCURS      27.
*
*計算領域
 01  WRK-AREA2.
     03  WRK-GOKSU           PIC  9(07)  VALUE ZERO.
*
 01  CNT-AREA.
     03  IX                  PIC  9(04)  VALUE  ZERO.
     03  IY                  PIC  9(04)  VALUE  ZERO.
     03  IZ                  PIC  9(04)  VALUE  ZERO.
     03  CNT-MEISAI          PIC  9(04)  VALUE  ZERO.
     03  CNT-PAGE            PIC  9(04)  VALUE  ZERO.
     03  CNT-MAXPAGE         PIC  9(04)  VALUE  ZERO.
     03  AMARI               PIC  9(04)  VALUE  ZERO.
*
 01  SEC-AREA.
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
*
 01  TABLE-AREA.
     03  TABLE-G.
         05  TABLE1      OCCURS  6.
             07  TBL-REC    PIC   X(200).
*
 COPY     FURIDOF   OF   XFDLIB   JOINING  WRK  PREFIX.
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
***************************************************************
 LINKAGE                    SECTION.
 01  PARA-BUMON             PIC X(04).
 01  PARA-TANTOU            PIC X(02).
*
****************************************************************
 PROCEDURE     DIVISION     USING   PARA-BUMON
                                    PARA-TANTOU.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー制御　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 画面ファイル >>--*
 DSPFILE-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DSPFILE.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SBZ0060I DSPFILE ERROR " DSP-CNTL " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     MOVE     "4000"     TO       PROGRAM-STATUS.
     STOP     RUN.
*----<< 振替移動実績ファイル >>----*
 FURIDOL1-ERR           SECTION.
     USE AFTER     EXCEPTION PROCEDURE      FURIDOL1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SBZ0060I FURIDOL1 ERROR " FURIDOL1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     MOVE     "4000"     TO       PROGRAM-STATUS.
     STOP     RUN.
*----<< 商品名称マスタ >>--*
 MEIMS1-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      MEIMS1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SBZ0060I MEIMS1 ERROR " MEIMS1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     MOVE     "4000"     TO       PROGRAM-STATUS.
     STOP     RUN.
*----<< 倉庫マスタ >>--*
 ZSOKMS1-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      ZSOKMS1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SBZ0060I ZSOKMS1 ERROR " ZSOKMS1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     MOVE     "4000"     TO       PROGRAM-STATUS.
     STOP     RUN.
*----<< 担当者マスタ >>--*
 TANMS1-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      TANMS1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SBZ0060I TANMS1 ERROR " TANMS1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     MOVE     "4000"     TO       PROGRAM-STATUS.
     STOP     RUN.
*----<< 条件ファイル >>--*
 JYOKEN1-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      JYOKEN1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SBZ0060I JYOKEN1  ERROR " JYOKEN1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     MOVE     "4000"     TO       PROGRAM-STATUS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     プログラムコントロール                      *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
*
     MOVE    "000-PROG-CNTL"      TO   S-NAME.
*
     PERFORM  100-INIT-RTN.
*
     PERFORM  200-MAIN-RTN   UNTIL     GR-NO    =    99.
*
     PERFORM  300-END-RTN.
*
*****STOP RUN.
     EXIT PROGRAM.
*
*
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      初期処理                                    *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
*
     MOVE    "100-INIT-RTN"       TO   S-NAME.
*
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
         MOVE LINK-OUT-YMD8  TO   WK-SYS-DATE
     ELSE
         MOVE ZERO           TO   SYS-DATEW
         MOVE ZERO           TO   WK-SYS-DATE
     END-IF.
*
     MOVE     SYS-YYW        TO   WK-SYSYY.
     MOVE     SYS-MMW        TO   WK-SYSMM.
     MOVE     SYS-DDW        TO   WK-SYSDD.
*
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SBZ0060I START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     I-O       DSPFILE.
     OPEN     I-O       FURIDOL1.
     OPEN     INPUT     TANMS1.
     OPEN     INPUT     MEIMS1.
     OPEN     INPUT     ZSOKMS1.
     OPEN     INPUT     JYOKEN1.
*
*条件Ｆ（経理月）
     MOVE     "58"           TO   JYO-F01.
     MOVE     SPACE          TO   JYO-F02.
     PERFORM  JYOKEN1-READ-SEC.
     IF       JYOKEN1-INV-FLG  =  "INV"
              DISPLAY   "HJYOKEN INV KEY=58"  UPON STAT
              MOVE      99        TO   GR-NO
              GO   TO   100-INIT-RTN-EXIT
     END-IF.
     MOVE     JYO-F04        TO   GETUDO.
     MOVE     JYO-F04        TO   GETUDO2.
*TEST↓
*    DISPLAY "-----------------------------" UPON CONS.
*    DISPLAY NC"取得：条件Ｆ（経理月）＝"  GETUDO   UPON CONS.
*    DISPLAY "-----------------------------" UPON CONS.
*TEST↑
*条件Ｆ（ＡＣＯＳ用締日）
     MOVE     "99"           TO   JYO-F01.
     MOVE     SPACE          TO   JYO-F02.
     PERFORM  JYOKEN1-READ-SEC.
     IF       JYOKEN1-INV-FLG  =  "INV"
              DISPLAY   "HJYOKEN INV KEY=99"  UPON STAT
              MOVE      99        TO   GR-NO
              GO   TO   100-INIT-RTN-EXIT
     END-IF.
     EVALUATE GETUDO
         WHEN 1
              MOVE      JYO-F04        TO   ACOS-DATE
         WHEN 2
              MOVE      JYO-F05        TO   ACOS-DATE
         WHEN 3
              MOVE      JYO-F06        TO   ACOS-DATE
         WHEN 4
              MOVE      JYO-F07        TO   ACOS-DATE
         WHEN 5
              MOVE      JYO-F08        TO   ACOS-DATE
         WHEN 6
              MOVE      JYO-F09        TO   ACOS-DATE
         WHEN 7
              MOVE      JYO-F10        TO   ACOS-DATE
         WHEN 8
              MOVE      JYO-F11        TO   ACOS-DATE
         WHEN 9
              MOVE      JYO-F12        TO   ACOS-DATE
         WHEN 10
              MOVE      JYO-F12A       TO   ACOS-DATE
         WHEN 11
              MOVE      JYO-F12B       TO   ACOS-DATE
         WHEN 12
              MOVE      JYO-F12C       TO   ACOS-DATE
         WHEN OTHER
              DISPLAY  "### ｹﾞﾂﾄﾞ ｲｼﾞｮｳ  ｹﾞﾂﾄﾞ= " GETUDO " ###"
                                       UPON CONS
              MOVE      99        TO   GR-NO
              GO   TO   100-INIT-RTN-EXIT
     END-EVALUATE.
*TEST↓
*    DISPLAY "-------------------------------------" UPON CONS.
*    DISPLAY NC"条件Ｆ（ＡＣＯＳ締日）＝"  ACOS-DATE UPON CONS.
*    DISPLAY "-------------------------------------" UPON CONS.
*TEST↑
*
     MOVE     ACOS-DATE      TO   GETUDO2-YMD.
     MOVE     99             TO   GETUDO2-DD.
     IF       GETUDO2        =    12
              COMPUTE        GETUDO2-YY     =
                             GETUDO2-YY     -        1
              MOVE     12    TO             GETUDO2-MM
     ELSE
              MOVE      GETUDO2   TO        GETUDO2-MM
     END-IF.
*
     MOVE     SYS-YY         TO   START-YY
                                  END-YY.
     MOVE     SYS-MM         TO   START-MM
                                  END-MM.
     ADD      1              TO   END-MM.
     IF       END-MM  >  12
              MOVE      1         TO   END-MM
              COMPUTE   END-YY   =  WK-SYS-YY  +  1
     ELSE
              MOVE      WK-SYS-YY   TO      END-YY
     END-IF.
     SUBTRACT 1              FROM START-MM.
     IF       START-MM  <  1
              MOVE      12        TO   START-MM
              COMPUTE   START-YY  =  WK-SYS-YY  -  1
     ELSE
              MOVE      WK-SYS-YY      TO   START-YY
     END-IF.
*
     MOVE     GETUDO2-YYMMR  TO      START-YYMM.
*
*TEST↓
*    DISPLAY "------------------------------" UPON CONS.
*    DISPLAY NC"算出：開始日付＝"  START-YYMM UPON CONS.
*    DISPLAY NC"算出：終了日付＝"  END-YYMM   UPON CONS.
*    DISPLAY "------------------------------" UPON CONS.
*TEST↑
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     MOVE     0              TO   GR-NO.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      メイン処理                                  *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*
     MOVE    "200-MAIN-RTN"       TO   S-NAME.
*
*----<< 画面初期表示 >>-*
     PERFORM  210-DSP-INIT   UNTIL     GR-NO    NOT  =    0.
*
*----<< モード　入力 >>-*
     PERFORM  220-INP-MODEGP UNTIL     GR-NO    NOT  =    1.
*
*----<< ヘッド　変更可部の入力 >>-*
     PERFORM  220-INP-KEYGRP UNTIL     GR-NO    NOT  =    2.
*
*----<< ヘッド　変更可部の入力 >>-*
     PERFORM  220-INP-CHGGRP UNTIL     GR-NO    NOT  =    3.
*
*----<< 明細    変更・入力 >>-*
     PERFORM  220-INP-BODY   UNTIL     GR-NO    NOT  =    4.
*
*----<< 確認入力 >>-*
     PERFORM  230-INP-ENDCHK UNTIL     GR-NO    NOT  =    9.
*
*----<< データ更新処理 >>-*
     PERFORM  240-KOUSIN-SEC UNTIL     GR-NO    NOT  =    10.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      終了処理                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
*
     MOVE    "300-END-RTN"        TO   S-NAME.
*
     CLOSE    DSPFILE.
     CLOSE    FURIDOL1.
     CLOSE    MEIMS1.
     CLOSE    ZSOKMS1.
     CLOSE    TANMS1.
     CLOSE    JYOKEN1.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SBZ0060I END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS     UPON CONS.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      画面初期表示                                *
*--------------------------------------------------------------*
 210-DSP-INIT           SECTION.
*
     MOVE    "210-DSP-INIT"       TO   S-NAME.
*
     MOVE     SPACE          TO   FBZ00601.
*
     PERFORM  CLR-SYKB-RTN.
     PERFORM  CLR-KEY-RTN.
     PERFORM  CLR-CHGGRP-RTN.
     PERFORM  CLR-BODY-RTN.
     PERFORM  CLR-END-RTN.
*
     MOVE    "SBZ0060I"      TO   PGID.
     MOVE    "FBZ00601"      TO   FORMID.
*    担当者
     MOVE     PARA-BUMON     TO   TAN-F01.
     MOVE     PARA-TANTOU    TO   TAN-F02.
     MOVE     PARA-BUMON     TO   BUMTAN(1:4).
     MOVE     "-"            TO   BUMTAN(5:1).
     MOVE     PARA-TANTOU    TO   BUMTAN(6:2).
     PERFORM  TANMS1-READ-SEC.
     IF  TANMS1-INV-FLG = "INV"
         MOVE ALL NC"＊"     TO   BUTANM
     ELSE
         MOVE TAN-F03        TO   BUTANM
     END-IF.
*
     MOVE     "M"            TO   EDIT-OPTION OF ERRMSG.
     MOVE     "M"            TO   EDIT-COLOR  OF ERRMSG.
*
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     "FBZ00601"     TO   DSP-FMT.
     MOVE     "ALLF"         TO   DSP-GRP.
     PERFORM  900-DSP-WRITE.
     MOVE     1              TO   GR-NO.
*
 210-DSP-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ヘッド　呼び出しＫＥＹ入力                  *
*--------------------------------------------------------------*
 220-INP-MODEGP         SECTION.
*
     MOVE     "220-INP-MODEGP"    TO   S-NAME.
*
     MOVE     "MODEGP"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF04
              MOVE      0         TO   GR-NO
              MOVE      SPACE     TO   CHK-FLG
         WHEN PF05
              MOVE      99        TO   GR-NO
         WHEN ENT
              PERFORM   CLR-SYKB-RTN
              PERFORM   CLR-KEY-RTN
              PERFORM   CLR-CHGGRP-RTN
              PERFORM   CLR-BODY-RTN
              PERFORM   CLR-END-RTN
              IF   SYKB  =  "2"  OR  "3"
                   MOVE      2    TO   GR-NO
              ELSE
                   MOVE      2    TO   ERR-FLG
                   MOVE     "C"   TO   EDIT-CURSOR OF SYKB
                   MOVE     "R"   TO   EDIT-OPTION OF SYKB
              END-IF
         WHEN OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 220-INP-MODEGP-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ヘッド　呼び出しＫＥＹ入力                  *
*--------------------------------------------------------------*
 220-INP-KEYGRP         SECTION.
*
     MOVE     "220-INP-KEYGRP"    TO   S-NAME.
*
     MOVE     "KEYGRP"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF04
              MOVE      0         TO   GR-NO
              MOVE      SPACE     TO   CHK-FLG
         WHEN PF05
              MOVE      99        TO   GR-NO
         WHEN PF06
              MOVE      1         TO   GR-NO
              PERFORM   CLR-KEY-RTN
         WHEN ENT
              PERFORM   CLR-SYKB-RTN
              PERFORM   CLR-KEY-RTN
              PERFORM   CLR-BODY-RTN
              PERFORM   CLR-END-RTN
              PERFORM   220-KEYGRP-CHECK-SEC
              MOVE      SPACE          TO   WK-KAKUN-FLG
              IF        ERR-FLG   =    ZERO
                   IF   SYKB  =  "2"
                        MOVE      3    TO   GR-NO
                   ELSE
                        MOVE      9    TO   GR-NO
                   END-IF
              END-IF
         WHEN OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 220-INP-KEYGRP-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＫＥＹチェック                              *
*--------------------------------------------------------------*
 220-KEYGRP-CHECK-SEC    SECTION.
*
     MOVE     "220-KEYGRP-CHECK-SEC"    TO   S-NAME.

     MOVE     ZERO      TO        ERR-FLG.
*伝票ＮＯチェック
*    　　　　未入力
     IF     ( DENNO     IS NOT NUMERIC ) OR
            ( DENNO     =      ZERO    )
              IF   ERR-FLG   =    ZERO
                   MOVE      3    TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF DENNO
              MOVE     "R"   TO   EDIT-OPTION OF DENNO
     END-IF.
*計上日チェック
*    　　　　未入力
     IF     ( KEIDAT    IS NOT NUMERIC ) OR
            ( KEIDAT    =      ZERO    )
              IF   ERR-FLG   =    ZERO
                   MOVE      5    TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF KEIDAT
              MOVE     "R"   TO   EDIT-OPTION OF KEIDAT
     END-IF.
*    　　　　異常日付
     IF     ( KEIDAT    IS NUMERIC     ) AND
            ( KEIDAT    NOT =     ZERO )
              MOVE     "2"        TO        LINK-IN-KBN
              MOVE      ZERO      TO        LINK-IN-YMD6
              MOVE      KEIDAT    TO        LINK-IN-YMD8
              MOVE      ZERO      TO        LINK-OUT-RET
              MOVE      ZERO      TO        LINK-OUT-YMD8
              CALL     "SKYDTCKB" USING     LINK-IN-KBN
                                            LINK-IN-YMD6
                                            LINK-IN-YMD8
                                            LINK-OUT-RET
                                            LINK-OUT-YMD8
              IF   LINK-OUT-RET   NOT =     ZERO
                   IF   ERR-FLG   =    ZERO
                        MOVE      6    TO   ERR-FLG
                   END-IF
                   MOVE     "C"   TO   EDIT-CURSOR OF KEIDAT
                   MOVE     "R"   TO   EDIT-OPTION OF KEIDAT
              END-IF
     END-IF.
*
*振替移動実績ファイル存在チェック
     IF  ERR-FLG        =    ZERO
         PERFORM   220-SONZAI-CHECK
         IF   ERR-FLG   =    ZERO
              MOVE      ZERO      TO   IX  WRK-GOKSU
              MOVE      SPACE     TO   WK-REC-ERR
*             画面セット
              PERFORM   220-GAMENST-SEC
         END-IF
     END-IF.
*
 220-KEYGRP-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      振替移動実績ファイル存在チェック            *
*--------------------------------------------------------------*
 220-SONZAI-CHECK       SECTION.
     MOVE    "220-SONZAI-CHECK"  TO   S-NAME.
*
     MOVE     SPACE          TO   FUR-REC.
     INITIALIZE                   FUR-REC.
*             伝票番号
     MOVE     DENNO          TO   FUR-F01.
*             行番号
     MOVE     1              TO   FUR-F02.
*             計上日（振替日）
     MOVE     KEIDAT         TO   FUR-F07.
*
     START    FURIDOL1  KEY  >=   FUR-F01   FUR-F02   FUR-F07
*        対象データなし
         INVALID   KEY
              MOVE      11   TO   ERR-FLG
              MOVE     "C"        TO   EDIT-CURSOR OF DENNO
              MOVE     "R"        TO   EDIT-OPTION OF DENNO
              MOVE     "R"        TO   EDIT-OPTION OF KEIDAT
              GO   TO   220-SONZAI-CHECK-EXIT
         NOT INVALID
              READ      FURIDOL1    NEXT
*               対象データなし
                AT END
                   MOVE      11   TO   ERR-FLG
                   MOVE     "C"   TO   EDIT-CURSOR OF DENNO
                   MOVE     "R"   TO   EDIT-CURSOR OF DENNO
                   MOVE     "R"   TO   EDIT-OPTION OF KEIDAT
                   GO   TO   220-SONZAI-CHECK-EXIT
                NOT AT END
*                  ＫＥＹチェック
                   IF ( DENNO      =    FUR-F01 ) AND
                      ( KEIDAT     =    FUR-F07 )
                        CONTINUE
                   ELSE
                        MOVE      11   TO   ERR-FLG
                        MOVE "C"  TO   EDIT-CURSOR OF DENNO
                        MOVE "R"  TO   EDIT-CURSOR OF DENNO
                        MOVE "R"  TO   EDIT-OPTION OF KEIDAT
                        GO   TO   220-SONZAI-CHECK-EXIT
                   END-IF
*                  計上済（計上区分・計上日）チェック
                   IF  ( FUR-F86       = "1" )
                        MOVE      8    TO   ERR-FLG
                        MOVE "C"  TO   EDIT-CURSOR OF DENNO
                        MOVE "R"  TO   EDIT-OPTION OF DENNO
                        MOVE "R"  TO   EDIT-OPTION OF KEIDAT
                        GO   TO   220-SONZAI-CHECK-EXIT
                   ELSE
                        CONTINUE
                   END-IF
              END-READ
     END-START.
 220-SONZAI-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      返品累積データ　ワークへの退避              *
*--------------------------------------------------------------*
 220-GAMENST-SEC          SECTION.
     MOVE     "220-GAMENST-SEC"     TO   S-NAME.
*
     IF     ( DENNO      =    FUR-F01 ) AND
            ( KEIDAT     =    FUR-F07 )
*           レコードカウント
              ADD       1         TO   IX
         IF  FUR-F02  =  1
*           ヘッド（変更可部）
*             変更計上日
              MOVE      FUR-F07   TO   HENDAT
*             倉庫ＣＤ
              MOVE      FUR-F04   TO   SOKCD  SOK-F01
              PERFORM ZSOKMS1-READ-SEC
              IF       ZSOKMS1-INV-FLG = "INV"
                       MOVE  ALL NC"＊"    TO   SOKNM
              ELSE
                       MOVE  SOK-F02       TO   SOKNM
              END-IF
*             振替元部門
              MOVE      22        TO   JYO-F01
              MOVE      FUR-F05   TO   FRMBCD  JYO-F02
              PERFORM JYOKEN1-READ-SEC
              IF       JYOKEN1-INV-FLG = "INV"
                       MOVE  ALL NC"＊"    TO   FRMBNM
              ELSE
                       MOVE  JYO-F03       TO   FRMBNM
              END-IF
*             振替先部門
              MOVE      22        TO   JYO-F01
              MOVE      FUR-F03   TO   FRSBCD  JYO-F02
              PERFORM JYOKEN1-READ-SEC
              IF       JYOKEN1-INV-FLG = "INV"
                       MOVE  ALL NC"＊"    TO   FRSBNM
              ELSE
                       MOVE  JYO-F03       TO   FRSBNM
              END-IF
         END-IF
*           明細
*             サカタ商品ＣＤ（サカタ商品ＣＤ）
              MOVE      FUR-F08   TO   MSYOCD  (IX) MEI-F01
*             品単１（サカタ品単１）
              MOVE      FUR-F09   TO   HINT1   (IX) MEI-F0121
*             品単２（サカタ品単２）
              MOVE      FUR-F10   TO   HINT2   (IX) MEI-F0122
*             品単３（サカタ品単３）
              MOVE      FUR-F11   TO   HINT3   (IX) MEI-F0123
              PERFORM  MEIMS1-READ-SEC
              IF       MEIMS1-INV-FLG = "INV"
                       MOVE  ALL "*"       TO   MSYONM(IX)
              ELSE
                       MOVE  MEI-F031      TO   MSYONM(IX)(1:15)
                       MOVE  MEI-F032(1:10) TO  MSYONM(IX)(16:10)
              END-IF
*             数量
              MOVE      FUR-F12   TO   MSURYO     (IX)
              ADD       FUR-F12   TO   WRK-GOKSU
*             備考
              MOVE      FUR-F14   TO   BIKO       (IX)
              MOVE      IX        TO   CNT-MEISAI
*             エラー区分
              MOVE      FUR-F83   TO   WK-REC-ERR
     ELSE
              GO        TO        220-GAMENST-090
     END-IF.
*
 220-GAMENST-010.
*
     READ     FURIDOL1  NEXT
         AT   END
              GO                  TO   220-GAMENST-090
         NOT AT END
              GO   TO   220-GAMENST-SEC
     END-READ.
*
 220-GAMENST-090.
     COMPUTE  IZ = CNT-MEISAI  +  1.
     PERFORM  VARYING IY FROM IZ BY 1  UNTIL  IY  >  6
              MOVE      "X"  TO   EDIT-STATUS OF MSYOCD (IY)
              MOVE      "X"  TO   EDIT-STATUS OF HINT1  (IY)
              MOVE      "X"  TO   EDIT-STATUS OF HINT2  (IY)
              MOVE      "X"  TO   EDIT-STATUS OF HINT3  (IY)
              MOVE      "X"  TO   EDIT-STATUS OF MSURYO (IY)
              MOVE      "X"  TO   EDIT-STATUS OF BIKO   (IY)
     END-PERFORM.
*エラー情報表示
     IF   WK-REC-ERR  =  "1"
          MOVE     NC"日付エラー有！"  TO  ERRMSG
          MOVE     "V"                 TO  EDIT-OPTION OF ERRMSG
          MOVE     "R"                 TO  EDIT-COLOR  OF ERRMSG
     ELSE
          MOVE     SPACE               TO  ERRMSG
          MOVE     "M"                 TO  EDIT-OPTION OF ERRMSG
          MOVE     "M"                 TO  EDIT-COLOR  OF ERRMSG
     END-IF.
*
     MOVE     WRK-GOKSU      TO   GOKSU.
*
 220-GAMENST-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      変更計上日入力　　　　　                    *
*--------------------------------------------------------------*
 220-INP-CHGGRP         SECTION.
*
     MOVE     "220-INP-CHGGRP"    TO   S-NAME.
*
     MOVE     "CHGGRP"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
     MOVE     ZERO      TO        ERR-FLG.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
         WHEN PF04
              MOVE      0         TO   GR-NO
              MOVE      SPACE     TO   CHK-FLG
         WHEN PF06
              PERFORM   CLR-CHGGRP-RTN
              PERFORM   CLR-BODY-RTN
              MOVE      2         TO   GR-NO
         WHEN ENT
              PERFORM   CLR-CHGGRP-RTN
**************PERFORM   CLR-BODY-RTN
              PERFORM   220-CHGGRP-CHECK-SEC
              IF        ERR-FLG   =    ZERO
                        MOVE      4    TO   GR-NO
              END-IF
         WHEN OTHER
              MOVE           1    TO   ERR-FLG
     END-EVALUATE.
*
 220-INP-CHGGRP-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ヘッド　変更可部　入力チェック              *
*--------------------------------------------------------------*
 220-CHGGRP-CHECK-SEC    SECTION.
*
     MOVE     "220-CHGGRP-CHECK-SEC"    TO   S-NAME.
*
     MOVE     ZERO      TO        ERR-FLG.
*
 220-CHGGRP-CHECK-03-01.
*変更計上日チェック
*   未入力
     IF     ( HENDAT    IS NOT NUMERIC ) OR
            ( HENDAT    =      ZERO    )
              IF   ERR-FLG   =    ZERO
                   MOVE      12   TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF HENDAT
              MOVE     "R"   TO   EDIT-OPTION OF HENDAT
     END-IF.
 220-CHGGRP-CHECK-03-02.
*   異常日付
     IF     ( HENDAT    IS NUMERIC     ) AND
            ( HENDAT    NOT =     ZERO )
              MOVE     "2"        TO        LINK-IN-KBN
              MOVE      ZERO      TO        LINK-IN-YMD6
              MOVE      HENDAT    TO        LINK-IN-YMD8
              MOVE      ZERO      TO        LINK-OUT-RET
              MOVE      ZERO      TO        LINK-OUT-YMD8
              CALL     "SKYDTCKB" USING     LINK-IN-KBN
                                            LINK-IN-YMD6
                                            LINK-IN-YMD8
                                            LINK-OUT-RET
                                            LINK-OUT-YMD8
              IF   LINK-OUT-RET   NOT =     ZERO
                   IF   ERR-FLG   =    ZERO
                        MOVE      6    TO   ERR-FLG
                   END-IF
                   MOVE     "C"   TO   EDIT-CURSOR OF HENDAT
                   MOVE     "R"   TO   EDIT-OPTION OF HENDAT
              END-IF
     END-IF.
*
 220-CHGGRP-CHECK-03-03.
     MOVE     HENDAT              TO   CHK-DATE.
*   規定範囲外日付１
*    DISPLAY "HENDAT       =  "  HENDAT       UPON CONS.
*    DISPLAY "CHK-YYMM     =  "  CHK-YYMM     UPON CONS.
*    DISPLAY "START-YYMM   =  "  START-YYMM   UPON CONS.
*    DISPLAY "END-YYMM     =  "  END-YYMM     UPON CONS.
     IF     ( HENDAT          NOT  =  0      )  AND
            ( CHK-YYMM    <   START-YYMM     )  OR
            ( CHK-YYMM    >   END-YYMM       )
              IF   ERR-FLG    =  ZERO
                   MOVE      13    TO   ERR-FLG
              END-IF
              MOVE "C"  TO   EDIT-CURSOR OF HENDAT
              MOVE "R"  TO   EDIT-OPTION OF HENDAT
              GO        TO    220-CHGGRP-CHECK-EXIT
     END-IF.
*
 220-CHGGRP-CHECK-03-04.
*   規定範囲外日付２
*    DISPLAY "ACOS-DATE    =  "  ACOS-DATE    UPON CONS.
*    DISPLAY "SYS-DATEW    =  "  SYS-DATEW    UPON CONS.
*    DISPLAY "CHK-YYMM     =  "  CHK-YYMM     UPON CONS.
     IF     ( HENDAT          NOT  =  0      )  AND
            ( ACOS-DATE       <  SYS-DATEW   )  AND
            ( CHK-YYMM        <  ACOS-YYMM   )
              IF   ERR-FLG    =  ZERO
                   MOVE       9    TO   ERR-FLG
              END-IF
              MOVE "C"  TO   EDIT-CURSOR OF HENDAT
              MOVE "R"  TO   EDIT-OPTION OF HENDAT
     END-IF.
*
 220-CHGGRP-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      明細　　変更・入力                          *
*--------------------------------------------------------------*
 220-INP-BODY           SECTION.
*
     MOVE     "220-INP-BODY"      TO   S-NAME.
*
     MOVE     "BODY"         TO   WK-GRP.
     PERFORM  900-DSP-READ.
     MOVE     ZERO      TO        ERR-FLG.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
         WHEN PF04
              MOVE      0         TO   GR-NO
              MOVE      SPACE     TO   CHK-FLG
         WHEN PF06
              PERFORM   CLR-CHGGRP-RTN
              PERFORM   CLR-BODY-RTN
              MOVE      3         TO   GR-NO
         WHEN ENT
              PERFORM   220-WRKTBL-CHECK-SEC
              IF        ERR-FLG   =    ZERO
                        MOVE      9    TO   GR-NO
                        PERFORM   CLR-CHGGRP-RTN
                        PERFORM   CLR-BODY-RTN
              END-IF
         WHEN OTHER
              MOVE           1    TO   ERR-FLG
     END-EVALUATE.
*
 220-INP-BODY-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      明細チェック　　　　　　　　　              *
*--------------------------------------------------------------*
 220-WRKTBL-CHECK-SEC    SECTION.
*
     MOVE     "220-WRKTBL-CHECK-SEC"    TO   S-NAME.
*
     MOVE     ZERO      TO        ERR-FLG  WRK-GOKSU.
*
*サカタコード→名称マスタ存在チェック
     PERFORM  VARYING  IZ   FROM  1  BY  1
                            UNTIL    IZ  >  CNT-MEISAI
              MOVE     MSYOCD   (IZ)     TO     MEI-F01
              MOVE     HINT1    (IZ)     TO     MEI-F0121
              MOVE     HINT2    (IZ)     TO     MEI-F0122
              MOVE     HINT3    (IZ)     TO     MEI-F0123
              PERFORM  MEIMS1-READ-SEC
              IF       MEIMS1-INV-FLG   =  "INV"
                  IF   ERR-FLG   =    ZERO
                       MOVE  10  TO   ERR-FLG
                       MOVE  "C" TO   EDIT-CURSOR OF MSYOCD(IZ)
                       MOVE  "R" TO   EDIT-OPTION OF MSYOCD(IZ)
                       MOVE  "R" TO   EDIT-OPTION OF HINT1 (IZ)
                       MOVE  "R" TO   EDIT-OPTION OF HINT2 (IZ)
                       MOVE  "R" TO   EDIT-OPTION OF HINT3 (IZ)
                  END-IF
              ELSE
                  IF   MSYONM  (IZ)  =    SPACE
                       MOVE  MEI-F031 TO   MSYONM(IZ)(1:15)
                       MOVE  MEI-F032 TO   MSYONM(IZ)(16:10)
                  END-IF
              END-IF
              ADD      MSURYO(IZ)     TO   WRK-GOKSU
     END-PERFORM.
*
     MOVE     WRK-GOKSU               TO   GOKSU.
*
 220-WRKTBL-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      確認入力　　　                              *
*--------------------------------------------------------------*
 230-INP-ENDCHK         SECTION.
*
     MOVE     "230-INP-ENDCHK"    TO   S-NAME.
*
     MOVE     "ENDCHK"            TO   WK-GRP.
     PERFORM   900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
         WHEN PF04
              MOVE      0         TO   GR-NO
              MOVE      SPACE     TO   CHK-FLG
         WHEN PF06
              MOVE      4         TO   GR-NO
         WHEN ENT
              MOVE      ZERO      TO   ERR-FLG
              MOVE      10        TO   GR-NO
         WHEN OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 230-INP-ENDCHK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    更新制御
*--------------------------------------------------------------*
 240-KOUSIN-SEC           SECTION.
*
     MOVE     "240-KOUSIN-SEC"    TO   S-NAME.
*
     IF   SYKB  =  "2"
          MOVE     SPACE    TO  TABLE-AREA
          PERFORM  240-DELETE-SEC
          MOVE     1        TO  IX
          PERFORM  240-WRITE-SEC
     ELSE
          PERFORM  240-DELETE-SEC
     END-IF.
*
     MOVE          0        TO   GR-NO.
*
 240-KOUSIN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    振替移動実績ファイル更新
*--------------------------------------------------------------*
 240-WRITE-SEC            SECTION.
*
     MOVE     "240-WRITE-SEC"     TO   S-NAME.
*
     IF   IX  >  CNT-MEISAI
          GO                      TO   240-WRITE-EXIT
     END-IF.
*
     MOVE      TBL-REC(IX)        TO   WRK-REC.
     IF    WRK-F01  =  ZERO
          GO                      TO   240-WRITE-EXIT
     END-IF.
*
     MOVE      SPACE              TO   FUR-REC.
     INITIALIZE                        FUR-REC.
*
     MOVE     WRK-REC         TO       FUR-REC.
*
     WRITE    FUR-REC.
     ADD      1               TO       IX.
     GO                       TO       240-WRITE-SEC.
*
 240-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    振替移動実績ファイル更新
*--------------------------------------------------------------*
 240-UPDATE-SEC           SECTION.
*
     MOVE     "240-UPDATE-SEC"    TO   S-NAME.
*
 240-UPDATE-00.
*
     MOVE     1              TO   IX.
*
     IF       IX             >    CNT-MEISAI
              GO             TO   240-UPDATE-99
     END-IF.
*
 240-UPDATE-02.
     MOVE     SPACE          TO   FUR-REC.
     INITIALIZE                   FUR-REC.
*             伝票番号　
     MOVE     DENNO          TO   FUR-F01.
*             行番号
     MOVE     1              TO   FUR-F02.
*             振替日
     MOVE     KEIDAT         TO   FUR-F07.
*
     START    FURIDOL1  KEY  >=   FUR-F01   FUR-F02   FUR-F07
*        対象データなし
         INVALID   KEY
              DISPLAY  "***********************************"
                                                     UPON CONS
              DISPLAY  "SBZ0060I  FURIDOL1 START INVALID ]]"
                                                     UPON CONS
              DISPLAY  "***********************************"
                                                     UPON CONS
              GO   TO   240-UPDATE-SEC-EXIT
     END-START.
*
 240-UPDATE-03.
*
     READ     FURIDOL1  NEXT
         AT END
              GO   TO   240-UPDATE-SEC-EXIT
         NOT AT END
*             DISPLAY "DENNO   1  = "  DENNO    UPON CONS
*             DISPLAY "FUR-F02 1  = "  FUR-F02  UPON CONS
*             DISPLAY "KEIDAT  1  = "  KEIDAT   UPON CONS
*             DISPLAY "FUR-F01 1  = "  FUR-F01  UPON CONS
*             DISPLAY "FUR-F07 1  = "  FUR-F07  UPON CONS
              IF ( DENNO          =    FUR-F01 ) AND
                 ( KEIDAT         =    FUR-F07 )
                   CONTINUE
              ELSE
                   GO   TO   240-UPDATE-SEC-EXIT
              END-IF
     END-READ.
*
 240-UPDATE-04.
*  振替日付
     MOVE     HENDAT          TO       FUR-F07.
*  サカタ商品ＣＤ
     MOVE     MSYOCD     (IX) TO       FUR-F08.
*  サカタ品単１
     MOVE     HINT1      (IX) TO       FUR-F09.
*  サカタ品単２
     MOVE     HINT2      (IX) TO       FUR-F10.
*  サカタ品単３
     MOVE     HINT3      (IX) TO       FUR-F11.
*  数量
     MOVE     MSURYO     (IX) TO       FUR-F12.
*  明細備考
     MOVE     BIKO       (IX) TO       FUR-F14.
*  エラー区分（初期化）
     MOVE     SPACE           TO       FUR-F83.
*  修正日付
     MOVE     SYS-DATEW       TO       FUR-F89.
*  修正担当者部門ＣＤ
     MOVE     PARA-BUMON      TO       FUR-F90.
*  修正担当者ＣＤ
     MOVE     PARA-TANTOU     TO       FUR-F91.
*  更新日付
     MOVE     SYS-DATEW       TO       FUR-F96.
*  更新時刻
     MOVE     SYS-TIME(1:6)   TO       FUR-F97.
*  更新担当者部門ＣＤ
     MOVE     PARA-BUMON      TO       FUR-F98.
*  更新担当者ＣＤ
     MOVE     PARA-TANTOU     TO       FUR-F99.
*
*    DISPLAY "FUR-F01 = " FUR-F01 UPON CONS.
*    DISPLAY "FUR-F02 = " FUR-F02 UPON CONS.
*    DISPLAY "FUR-F07 = " FUR-F07 UPON CONS.
     REWRITE  FUR-REC.
     ADD      1         TO        IX.
     GO                 TO        240-UPDATE-03.
*
 240-UPDATE-99.
*画面初期化ＳＥＣへ
     MOVE     0                   TO   GR-NO.
*
 240-UPDATE-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    振替移動実績ファイル削除
*--------------------------------------------------------------*
 240-DELETE-SEC           SECTION.
*
     MOVE     "240-DELETE-SEC"    TO   S-NAME.
*
 240-DELETE-00.
*
     MOVE     1              TO   IX.
*
     IF       IX             >    CNT-MEISAI
              GO             TO   240-DELETE-99
     END-IF.
*
 240-DELETE-02.
     MOVE     SPACE          TO   FUR-REC.
     INITIALIZE                   FUR-REC.
*             伝票番号　
     MOVE     DENNO          TO   FUR-F01.
*             行番号
     MOVE     1              TO   FUR-F02.
*             振替日
     MOVE     KEIDAT         TO   FUR-F07.
*
     START    FURIDOL1  KEY  >=   FUR-F01   FUR-F02   FUR-F07
*        対象データなし
         INVALID   KEY
              DISPLAY  "***********************************"
                                                     UPON CONS
              DISPLAY  "SBZ0060I  FURIDOL1 START INVALID ]]"
                                                     UPON CONS
              DISPLAY  "***********************************"
                                                     UPON CONS
              GO   TO   240-DELETE-SEC-EXIT
     END-START.
*
 240-DELETE-03.
*
     READ     FURIDOL1  NEXT
         AT END
              GO   TO   240-DELETE-SEC-EXIT
         NOT AT END
              IF ( DENNO          =    FUR-F01 ) AND
                 ( KEIDAT         =    FUR-F07 )
                   CONTINUE
              ELSE
                   GO   TO   240-DELETE-SEC-EXIT
              END-IF
     END-READ.
*
 240-DELETE-04.
*
*  振替日付
     MOVE     HENDAT          TO       FUR-F07.
*  サカタ商品ＣＤ
     MOVE     MSYOCD     (IX) TO       FUR-F08.
*  サカタ品単１
     MOVE     HINT1      (IX) TO       FUR-F09.
*  サカタ品単２
     MOVE     HINT2      (IX) TO       FUR-F10.
*  サカタ品単３
     MOVE     HINT3      (IX) TO       FUR-F11.
*  数量
     MOVE     MSURYO     (IX) TO       FUR-F12.
*  明細備考
     MOVE     BIKO       (IX) TO       FUR-F14.
*  エラー区分（初期化）
     MOVE     SPACE           TO       FUR-F83.
*  修正日付
     MOVE     SYS-DATEW       TO       FUR-F89.
*  修正担当者部門ＣＤ
     MOVE     PARA-BUMON      TO       FUR-F90.
*  修正担当者ＣＤ
     MOVE     PARA-TANTOU     TO       FUR-F91.
*  更新日付
     MOVE     SYS-DATEW       TO       FUR-F96.
*  更新時刻
     MOVE     SYS-TIME(1:6)   TO       FUR-F97.
*  更新担当者部門ＣＤ
     MOVE     PARA-BUMON      TO       FUR-F98.
*  更新担当者ＣＤ
     MOVE     PARA-TANTOU     TO       FUR-F99.
     MOVE     FUR-REC   TO        TBL-REC(IX).
     DELETE   FURIDOL1.
     ADD      1         TO        IX.
     GO                 TO        240-DELETE-03.
*
 240-DELETE-99.
*画面初期化ＳＥＣへ
     MOVE     0                   TO   GR-NO.
*
 240-DELETE-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     画面　ＲＥＡＤ                              *
*--------------------------------------------------------------*
 900-DSP-READ           SECTION.
*
     MOVE     "900-DSP-READ"      TO   S-NAME.
*
     MOVE     "ALLF"         TO   DSP-GRP.
     IF       ERR-FLG   =    0
              MOVE      SPACE               TO   MSGSPC
              MOVE      "D"      TO   EDIT-OPTION OF MSGSPC
     ELSE
              MOVE      MSG-TBL (ERR-FLG)   TO   MSGSPC
     END-IF.
     IF       GR-NO     =    1
              MOVE      GUIDE00   TO   FNCSPC
     ELSE
              IF   GR-NO     =    2  OR  3
                   MOVE      GUIDE01   TO   FNCSPC
              END-IF
              IF   GR-NO     =    4
                   MOVE      GUIDE02   TO   FNCSPC
              END-IF
     END-IF.
     MOVE     SYS-DATEW           TO   SDATE.
     ACCEPT   SYS-TIME2           FROM TIME.
     MOVE     SYS-TIMEW           TO   STIME.
*
     PERFORM  900-DSP-WRITE.
*
     IF       MSGSPC    NOT  =    SPACE
              MOVE "AL"      TO   DSP-PRO
     ELSE
              MOVE "NE"      TO   DSP-PRO
     END-IF.
     MOVE     SPACE          TO   MSGSPC.
*
     MOVE     WK-GRP         TO   DSP-GRP.
     READ     DSPFILE.
     MOVE     SPACE          TO   DSP-PRO.
*
 900-DSP-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     画面　ＷＲＩＴＥ                            *
*--------------------------------------------------------------*
 900-DSP-WRITE          SECTION.
*
     MOVE     "900-DSP-WRITE"     TO   S-NAME.
*
     MOVE     SPACE               TO   DSP-PRO.
     WRITE    FBZ00601.
*
 900-DSP-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＨＥＡＤ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-SYKB-RTN           SECTION.
     MOVE     " "            TO   EDIT-CURSOR OF SYKB.
     MOVE     "M"            TO   EDIT-OPTION OF SYKB.
 CLR-SYKB-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＨＥＡＤ　変更部　属性クリア　　　　　　　　*
*--------------------------------------------------------------*
 CLR-KEY-RTN           SECTION.
     MOVE     " "            TO   EDIT-CURSOR OF DENNO
                                  EDIT-CURSOR OF KEIDAT
                                  EDIT-CURSOR OF HENDAT.
     MOVE     "M"            TO   EDIT-OPTION OF DENNO
                                  EDIT-OPTION OF KEIDAT
                                  EDIT-OPTION OF HENDAT.
 CLR-KEY-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＨＥＡＤ　変更部　属性クリア　　　　　　　　*
*--------------------------------------------------------------*
 CLR-CHGGRP-RTN        SECTION.
     MOVE     " "            TO   EDIT-CURSOR OF HENDAT.
     MOVE     "M"            TO   EDIT-OPTION OF HENDAT.
 CLR-CHGGRP-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＢＯＤＹ属性クリア　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-BODY-RTN          SECTION.
*
     PERFORM  VARYING   IX   FROM  1   BY   1    UNTIL  IX > 6
         MOVE     " "        TO   EDIT-CURSOR OF MSYOCD(IX)
         MOVE     "M"        TO   EDIT-OPTION OF MSYOCD(IX)
         MOVE     " "        TO   EDIT-STATUS OF MSYOCD(IX)
         MOVE     " "        TO   EDIT-CURSOR OF HINT1(IX)
         MOVE     "M"        TO   EDIT-OPTION OF HINT1(IX)
         MOVE     " "        TO   EDIT-STATUS OF HINT1(IX)
         MOVE     " "        TO   EDIT-CURSOR OF HINT2(IX)
         MOVE     "M"        TO   EDIT-OPTION OF HINT2(IX)
         MOVE     " "        TO   EDIT-STATUS OF HINT2(IX)
         MOVE     " "        TO   EDIT-CURSOR OF HINT3(IX)
         MOVE     "M"        TO   EDIT-OPTION OF HINT3(IX)
         MOVE     " "        TO   EDIT-STATUS OF HINT3(IX)
         MOVE     " "        TO   EDIT-CURSOR OF MSYONM(IX)
         MOVE     "M"        TO   EDIT-OPTION OF MSYONM(IX)
         MOVE     " "        TO   EDIT-STATUS OF MSYONM(IX)
         MOVE     " "        TO   EDIT-CURSOR OF MSURYO(IX)
         MOVE     "M"        TO   EDIT-OPTION OF MSURYO(IX)
         MOVE     " "        TO   EDIT-STATUS OF MSURYO(IX)
         MOVE     " "        TO   EDIT-CURSOR OF BIKO(IX)
         MOVE     "M"        TO   EDIT-OPTION OF BIKO(IX)
         MOVE     " "        TO   EDIT-STATUS OF BIKO(IX)
     END-PERFORM.
*
 CLR-BODY-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＥＮＤ　　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-END-RTN           SECTION.
     MOVE     " "        TO   EDIT-CURSOR OF ENDCHK.
     MOVE     "M"        TO   EDIT-OPTION OF ENDCHK.
 CLR-END-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    条件ファイル　読込
*--------------------------------------------------------------*
 JYOKEN1-READ-SEC       SECTION.
*
     READ  JYOKEN1
           INVALID
           MOVE  "INV"  TO   JYOKEN1-INV-FLG
           NOT  INVALID
           MOVE  SPACE  TO   JYOKEN1-INV-FLG
     END-READ.
*
 JYOKEN1-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    倉庫マスタ　読込
*--------------------------------------------------------------*
 ZSOKMS1-READ-SEC       SECTION.
*
     READ  ZSOKMS1
           INVALID
           MOVE  "INV"  TO   ZSOKMS1-INV-FLG
           NOT  INVALID
           MOVE  SPACE  TO   ZSOKMS1-INV-FLG
     END-READ.
*
 ZSOKMS1-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    商品名称マスタ読込
*--------------------------------------------------------------*
 MEIMS1-READ-SEC       SECTION.
*
     READ  MEIMS1
           INVALID
           MOVE  "INV"  TO   MEIMS1-INV-FLG
           NOT  INVALID
           MOVE  SPACE  TO   MEIMS1-INV-FLG
     END-READ.
*
 MEIMS1-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    担当者マスタ読込
*--------------------------------------------------------------*
 TANMS1-READ-SEC       SECTION.
*
     READ  TANMS1
           INVALID
           MOVE  "INV"  TO   TANMS1-INV-FLG
           NOT  INVALID
           MOVE  SPACE  TO   TANMS1-INV-FLG
     END-READ.
*
 TANMS1-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
