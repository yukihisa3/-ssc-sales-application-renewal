****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理システム　　　　　　　　　*
*    業務名　　　　　　　：　伝票訂正入力                      *
*    モジュール名　　　　：　伝票訂正入力                      *
*    作成日／更新日　　　：　99/09/27                          *
*    作成者／更新者　　　：　ＮＡＶ吉田　　　　　　　　　　　　*
*    処理概要　　　　　　：　画面より、バッチ_、伝票_を入    *
*                            力し、該当の伝票を表示し、数量    *
*                            の訂正を行う。                    *
*　　　　　　　　　　　　　　                                  *
*　                                                            *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY0401I.
 AUTHOR.                NAV Y.YOSHIDA.
 DATE-WRITTEN.          99/09/24.
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
*----<< 伝票データ >>--*
     SELECT   SHTDENL1  ASSIGN    TO        DA-01-VI-SHTDENL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
                        RECORD    KEY  DEN-F01   DEN-F02
                                       DEN-F04   DEN-F051
                                       DEN-F03
                        FILE      STATUS    IS   SHTDENL1-ST.
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TOK-F01
                        FILE      STATUS    IS   HTOKMS-ST.
*---<<  商品コード変換テーブル  >>---*
     SELECT   HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SHO-F01
                                                 SHO-F02
                        FILE      STATUS    IS   SHO-ST.
*---<<  商品名称マスタ  >>---*
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   MEI-F01
                        FILE      STATUS    IS   MEI-ST.
*----<< 店舗マスタ >>-*
     SELECT   TENMS1    ASSIGN         DA-01-VI-TENMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TEN-F52   TEN-F011
                        STATUS         TENMS1-ST.
*---<<  商品在庫マスタ  >>---*
     SELECT   ZZAIMS    ASSIGN    TO        DA-01-VI-ZZAIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   ZAI-F01
                                                 ZAI-F02
                                                 ZAI-F03
                        FILE      STATUS    IS   ZAI-ST.
*
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 表示ファイル >>--*
 FD  DSPFILE            LABEL     RECORD   IS   STANDARD.
     COPY     FSY04011  OF        XMDLIB.
*----<< 伝票データ >>--*
 FD  SHTDENL1           LABEL     RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN       PREFIX.
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< 店舗マスタ >>--*
 FD  TENMS1             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*---<<  商品変換テーブル  >>---*
 FD  HSHOTBL.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   SHO       PREFIX.
*---<<  商品名称マスタ  >>---*
 FD  HMEIMS.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
*---<<  商品在庫マスタ  >>---*
 FD  ZZAIMS.
     COPY     ZZAIMS    OF        XFDLIB
              JOINING   ZAI       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  ERR-FLG        PIC  9(02)    VALUE  ZERO.
     03  UPD-FLG        PIC  9(01)    VALUE  ZERO.
     03  SYO-FLG        PIC  9(01)    VALUE  ZERO.
     03  KEP-FLG             PIC  X(01)  VALUE  SPACE.
     03  HSHOTBL-INV-FLG     PIC  X(03)  VALUE SPACE.
     03  HMEIMS-INV-FLG      PIC  X(03)  VALUE SPACE.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SHTDENL1-ST       PIC  X(02).
 01  HTOKMS-ST         PIC  X(02).
 01  TENMS1-ST         PIC  X(02).
 01  SHO-ST            PIC  X(02).
 01  MEI-ST            PIC  X(02).
 01  ZAI-ST            PIC  X(02).
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
     03  PF11           PIC  X(04)     VALUE     "F011".
     03  PF12           PIC  X(04)     VALUE     "F012".
*
*----<< ﾌｱﾝｸｼﾖﾝ ｷｰ ｶﾞｲﾄﾞ >>-*
 01  GUIDE01       PIC  N(40)  VALUE   NC"_取消　_終　了".
 01  GUIDE02       PIC  N(40)  VALUE
     NC"_取　消　_終　了　_項目戻り".
 01  GUIDE03       PIC  N(40)  VALUE
     NC"_取　消　_終　了　_項目戻り　_次　頁".
 01  GUIDE04       PIC  N(40)  VALUE
     NC"_取　消　_終　了　_項目戻り　_前　頁".
*
 01  MSG-AREA.
     03  MSG01               PIC  N(30)  VALUE
              NC"ＰＦキーが違います".
     03  MSG02               PIC  N(30)  VALUE
              NC"バッチ_を入力して下さい。".
     03  MSG03               PIC  N(30)  VALUE
              NC"バッチ_に誤りがあります。".
     03  MSG04               PIC  N(30)  VALUE
              NC"取引先コードが違います。".
     03  MSG05               PIC  N(30)  VALUE
              NC"売上伝票データに存在しません。".
     03  MSG06               PIC  N(30)  VALUE
              NC"取引先コードを入力して下さい。".
     03  MSG07               PIC  N(30)  VALUE
              NC"訂正伝票_を入力して下さい。".
     03  MSG08               PIC  N(30)  VALUE
              NC"訂正数量が違います。".
     03  MSG09               PIC  N(30)  VALUE
              NC"　".
     03  MSG10               PIC  N(30)  VALUE
              NC"前頁はありません。".
     03  MSG11               PIC  N(30)  VALUE
              NC"次頁はありません。".
     03  MSG12               PIC  N(30)  VALUE
              NC"売上済の為、修正できません。".
     03  MSG13               PIC  N(30)  VALUE
              NC"Ｙを入力".
     03  MSG14               PIC  N(30)  VALUE
              NC"訂正数量が発注数量を超えています。".
*
 01  FILLER                  REDEFINES   MSG-AREA.
     03  MSG-TBL             PIC  N(30)  OCCURS      14.
*
 01  WRK-AREA.
     03  WRK-TENPO           PIC  9(05).
     03  WRK-TENPNM          PIC  N(15).
     03  WRK-BUNRUI          PIC  X(04).
     03  WRK-DKU             PIC  X(02).
     03  WRK-HYMD            PIC  9(08).
     03  WRK-NYMD            PIC  9(08).
     03  WRK-MEISAI          OCCURS    12.
       05  WRK-GYO           PIC  9(02).
       05  WRK-RYOCD         PIC  X(13).
       05  WRK-SMEI1         PIC  X(15).
       05  WRK-SMEI2         PIC  X(15).
       05  WRK-JSHOCD        PIC  X(08).
       05  WRK-HINTAN.
         07  WRK-HTAN1       PIC  X(05).
         07  WRK-HTAN2       PIC  X(02).
         07  WRK-HTAN3       PIC  X(01).
       05  WRK-HATSUU        PIC S9(09)V99.
       05  WRK-TEISUU        PIC S9(09)V99.
       05  WRK-GENTAN        PIC S9(09)V99.
       05  WRK-GKIN          PIC S9(09)V99.
       05  WRK-MAETEISUU     PIC S9(09)V99.
       05  WRK-URISAK        PIC  9(01).
     03  WRK-HAKEI           PIC S9(09)V99.
     03  WRK-TEKEI           PIC S9(09)V99.
*キー部退避
 01  WRK-KEY.
     03  WRK-JDATE           PIC  9(08)        VALUE ZERO.
     03  WRK-JTIME           PIC  9(04)        VALUE ZERO.
     03  WRK-TORICD          PIC  9(08)        VALUE ZERO.
*計算領域
 01  WRK-AREA2.
     03  WRK-HIK             PIC S9(09)V9(02)  VALUE ZERO.
     03  WRK-ZAI             PIC S9(09)V9(02)  VALUE ZERO.
*
 01  BRK-AREA.
     03  BRK-DENNO           PIC  9(09)  VALUE  ZERO.
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
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
****************************************************************
 PROCEDURE              DIVISION.
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
     DISPLAY  "### SSY0401I DSPFILE ERROR " DSP-CNTL " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENL1  HTOKMS    TENMS1    DSPFILE   ZZAIMS
              HSHOTBL   HMEIMS.
     STOP     RUN.
*----<< 伝票データ >>--*
 SHTDENL1-ERR           SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHTDENL1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY0401I SHTDENL1 ERROR " SHTDENL1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENL1  HTOKMS    TENMS1    DSPFILE   ZZAIMS
              HSHOTBL   HMEIMS.
     STOP     RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY0401I HTOKMS ERROR " HTOKMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENL1  HTOKMS    TENMS1    DSPFILE   ZZAIMS
              HSHOTBL   HMEIMS.
     STOP     RUN.
*----<< 店舗マスタ >>--*
 TENMS1-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      TENMS1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY0301I TENMS1 ERROR " TENMS1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENL1  HTOKMS    TENMS1    DSPFILE   ZZAIMS
              HSHOTBL   HMEIMS.
     STOP     RUN.
*----<< 商品コード変換テーブル >>--*
 HSHOTBL-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HSHOTBL.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY0301I HSHOTBL ERROR " SHO-ST    " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENL1  HTOKMS    TENMS1    DSPFILE   ZZAIMS
              HSHOTBL   HMEIMS.
     STOP     RUN.
*----<< 商品名称マスタ >>--*
 HMEIMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HMEIMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY0301I HMEIMS  ERROR " MEI-ST    " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENL1  HTOKMS    TENMS1    DSPFILE   ZZAIMS
              HSHOTBL   HMEIMS.
     STOP     RUN.
*----<< 在庫マスタ >>--*
 ZZAIMS-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      ZZAIMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY0301I ZZAIMS ERROR " ZAI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENL1  HTOKMS    TENMS1    DSPFILE   ZZAIMS
              HSHOTBL   HMEIMS.
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
*
     INITIALIZE                   WRK-AREA.
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
     DISPLAY  "*** SSY0401I START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     I-O       DSPFILE.
     OPEN     I-O       SHTDENL1.
     OPEN     I-O       ZZAIMS.
     OPEN     INPUT     HTOKMS.
     OPEN     INPUT     TENMS1.
     OPEN     INPUT     HSHOTBL.
     OPEN     INPUT     HMEIMS.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     MOVE     0              TO   WRK-KEY.
     MOVE     0              TO   GR-NO.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     MOVE    "200-MAIN-RTN"       TO   S-NAME.
*----<< ﾆｭｳﾘｮｸ ｶﾞﾒﾝ ｸﾘｱ >>-*
     PERFORM  210-DSP-INIT   UNTIL     GR-NO    NOT  =    0.
*----<< ﾊﾞｯﾁNO. ﾆｭｳﾘｮｸ >>-*
     PERFORM  220-INP-GRP01  UNTIL     GR-NO    NOT  =    1.
*----<< ﾒｲｻｲ    ﾆｭｳﾘｮｸ >>-*
     PERFORM  220-INP-GRP02  UNTIL     GR-NO    NOT  =    2.
*----<< ｶｸﾆﾝ ﾆｭｳﾘｮｸ >>-*
     PERFORM  230-INP-KKNN   UNTIL     GR-NO    NOT  =    9.
*----<< ｳﾘｱｹﾞ ﾃﾞﾝﾋﾟｮｳ ｺｳｼﾝ ｼｮﾘ >>-*
     PERFORM  240-UPDATE-SEC UNTIL     GR-NO    NOT  =    10.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     MOVE    "300-END-RTN"        TO   S-NAME.
     CLOSE    DSPFILE.
     CLOSE    SHTDENL1.
     CLOSE    HTOKMS.
     CLOSE    TENMS1.
     CLOSE    HSHOTBL.
     CLOSE    HMEIMS.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSY0401I END *** "
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
     MOVE     SPACE          TO   FSY04011.
*
     PERFORM  CLR-HEAD-RTN.
     PERFORM  CLR-BODY1-RTN.
     PERFORM  CLR-TAIL-RTN.
*
     MOVE    "SSY0401I"      TO   PGID.
     MOVE    "FSY04011"      TO   FORM.
*
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     "FSY04011"     TO   DSP-FMT.
     MOVE     "SCREFX"       TO   DSP-GRP.
*キー部継続使用時
     IF        WRK-JDATE  =  ZERO
     AND       WRK-JTIME  =  ZERO
     AND       WRK-TORICD =  ZERO
               MOVE    " "       TO   EDIT-STATUS OF JDATE
               MOVE    " "       TO   EDIT-STATUS OF JTIME
               MOVE    " "       TO   EDIT-STATUS OF TORICD
     ELSE
               MOVE    "X"       TO   EDIT-STATUS OF JDATE
               MOVE    "X"       TO   EDIT-STATUS OF JTIME
               MOVE    "X"       TO   EDIT-STATUS OF TORICD
               MOVE    WRK-JDATE TO   JDATE
               MOVE    WRK-JTIME TO   JTIME
               MOVE    WRK-TORICD TO  TORICD
     END-IF.
     PERFORM  900-DSP-WRITE.
*
     MOVE     1              TO   GR-NO.
 210-DSP-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾞｯﾁNO.  ﾆｭｳﾘｮｸ                             *
*--------------------------------------------------------------*
 220-INP-GRP01          SECTION.
     MOVE     "220-INP-GRP01"     TO   S-NAME.
     MOVE     "GRP001"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF04
              MOVE      ZERO      TO   GR-NO  WRK-KEY
         WHEN PF05
              MOVE      99        TO   GR-NO
         WHEN ENT
              INITIALIZE               WRK-AREA
              INITIALIZE               CNT-AREA
              PERFORM   CLR-HEAD-RTN
              PERFORM   CLR-BODY1-RTN
              PERFORM   220-GRP01-CHECK-SEC
              IF        ERR-FLG   =    ZERO
*                       画面セット処理
                        MOVE      1    TO   CNT-PAGE
                        PERFORM   GAMEN-SET-SEC
                        MOVE      JDATE  TO  WRK-JDATE
                        MOVE      JTIME  TO  WRK-JTIME
                        MOVE      TORICD TO  WRK-TORICD
                        IF   SYO-FLG   =    1
                             MOVE      9    TO   GR-NO
                             MOVE      12   TO   ERR-FLG
                        ELSE
                             MOVE      2    TO   GR-NO
                        END-IF
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
     IF     ( JDATE     =    ZERO ) AND
            ( JTIME     =    ZERO ) AND
            ( TORICD    =    ZERO )
              MOVE      2         TO   ERR-FLG
              MOVE     "C"        TO   EDIT-CURSOR OF JDATE
              MOVE     "R"        TO   EDIT-OPTION OF JDATE
              MOVE     "R"        TO   EDIT-OPTION OF JTIME
              MOVE     "R"        TO   EDIT-OPTION OF TORICD
     END-IF.
*    受信日付チェック
     IF     ( JDATE     IS NUMERIC     ) AND
            ( JDATE     NOT =     ZERO )
              MOVE     "2"        TO        LINK-IN-KBN
              MOVE      JDATE     TO        LINK-IN-YMD8
              CALL     "SKYDTCKB" USING     LINK-IN-KBN
                                            LINK-IN-YMD6
                                            LINK-IN-YMD8
                                            LINK-OUT-RET
                                            LINK-OUT-YMD8
              IF   LINK-OUT-RET   NOT =     ZERO
                   IF   ERR-FLG   =    ZERO
                        MOVE      3    TO   ERR-FLG
                   END-IF
                   MOVE     "C"   TO   EDIT-CURSOR OF JDATE
                   MOVE     "R"   TO   EDIT-OPTION OF JDATE
              END-IF
     END-IF.
*    受信時間
     IF  JTIME     NOT NUMERIC
         MOVE      ZERO      TO   JTIME
     END-IF.
*    取引先チェック
     IF  TORICD    IS NUMERIC     AND
         TORICD    NOT =     ZERO
         MOVE      SPACE     TO   TOK-REC
         INITIALIZE               TOK-REC
         MOVE      TORICD    TO   TOK-F01
         READ      HTOKMS
             INVALID
                   MOVE      SPACE     TO   TORINM
                   IF   ERR-FLG   =    ZERO
                        MOVE      4    TO   ERR-FLG
                   END-IF
                   MOVE     "C"   TO   EDIT-CURSOR OF TORICD
                   MOVE     "R"   TO   EDIT-OPTION OF TORICD
             NOT INVALID
                   MOVE      TOK-F02   TO   TORINM
         END-READ
     ELSE
         IF   ERR-FLG   =    ZERO
              MOVE      6    TO   ERR-FLG
         END-IF
         MOVE     "C"   TO   EDIT-CURSOR OF TORICD
         MOVE     "R"   TO   EDIT-OPTION OF TORICD
     END-IF.
*    伝票_チェック
     IF       DENNO     IS   NUMERIC
              IF   DENNO     NOT =     ZERO
                   CONTINUE
              ELSE
                   IF   ERR-FLG   =    ZERO
                        MOVE      7    TO   ERR-FLG
                   END-IF
                   MOVE     "C"   TO   EDIT-CURSOR OF DENNO
                   MOVE     "R"   TO   EDIT-OPTION OF DENNO
              END-IF
     ELSE
              IF        ERR-FLG   =    ZERO
                        MOVE      7    TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF DENNO
              MOVE     "R"   TO   EDIT-OPTION OF DENNO
     END-IF.
*    売上伝票データ存在チェック
     IF  ERR-FLG        =    ZERO
         PERFORM   220-SONZAI-CHECK
         IF   ERR-FLG   =    ZERO
              MOVE      ZERO      TO   IX
*             対象データ退避処理
              PERFORM   220-TAIHI-SEC
              IF        CNT-MEISAI     =    ZERO
                   IF   ERR-FLG   =    ZERO
                        MOVE      5    TO   ERR-FLG
                   END-IF
                   MOVE     "C"   TO   EDIT-CURSOR OF JDATE
                   MOVE     "R"   TO   EDIT-OPTION OF JDATE
                   MOVE     "R"   TO   EDIT-OPTION OF JTIME
                   MOVE     "R"   TO   EDIT-OPTION OF TORICD
                   MOVE     "R"   TO   EDIT-OPTION OF DENNO
                   GO        TO   220-GRP01-CHECK-EXIT
              END-IF
              IF        CNT-MEISAI     <=   6
                   MOVE      1         TO   CNT-MAXPAGE
              ELSE
                   DIVIDE    CNT-MEISAI     BY   6
                             GIVING    CNT-MAXPAGE
                             REMAINDER      AMARI
                   IF   AMARI     NOT =     ZERO
                        ADD  1    TO   CNT-MAXPAGE
                   END-IF
              END-IF
         END-IF
     END-IF.
*
 220-GRP01-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｶﾞﾒﾝ ﾒｲｻｲ ｾｯﾄ                               *
*--------------------------------------------------------------*
 GAMEN-SET-SEC          SECTION.
     MOVE    "GAMEN-SET-SEC"      TO   S-NAME.
*
     MOVE     WRK-TENPO      TO   TENPO.
     MOVE     WRK-TENPNM     TO   TENPNM.
     MOVE     WRK-BUNRUI     TO   BUNRUI.
     MOVE     WRK-DKU        TO   DENKU.
     MOVE     WRK-HYMD       TO   HACYMD.
     MOVE     WRK-NYMD       TO   NOUYMD.
*
     COMPUTE  IY   =    CNT-PAGE  *    6   -     6.
     PERFORM  VARYING   IZ   FROM  1   BY   1    UNTIL IZ  >  6
         ADD  1    TO   IY
         IF   WRK-GYO(IY)    NOT =     ZERO
              MOVE      SPACE          TO   MAS001(IZ)
              MOVE      WRK-GYO   (IY) TO   GYO   (IZ)
              MOVE      WRK-RYOCD (IY) TO   RYOCD (IZ)
              MOVE      WRK-SMEI1 (IY) TO   SMEI1 (IZ)
              MOVE      WRK-SMEI2 (IY) TO   SMEI2 (IZ)
              MOVE      WRK-JSHOCD(IY) TO   JSHOCD(IZ)
              MOVE      WRK-HTAN1 (IY) TO   HTAN1 (IZ)
              MOVE      WRK-HTAN2 (IY) TO   HTAN2 (IZ)
              MOVE      WRK-HTAN3 (IY) TO   HTAN3 (IZ)
              MOVE      WRK-HATSUU(IY) TO   HATSUU(IZ)
              MOVE      WRK-TEISUU(IY) TO   TEISUU(IZ)
              MOVE      WRK-GENTAN(IY) TO   GENTAN(IZ)
              COMPUTE   GKIN(IZ)  =    WRK-TEISUU(IY) *
                                       WRK-GENTAN(IY)
              MOVE      " "  TO   EDIT-STATUS OF TEISUU(IZ)
         ELSE
              MOVE      SPACE          TO   MAS001(IZ)
              MOVE      "X"  TO   EDIT-STATUS OF TEISUU(IZ)
         END-IF
     END-PERFORM.
*
*    合計計算
     MOVE     ZERO           TO   WRK-TEKEI.
     MOVE     ZERO           TO   SYO-FLG.
     PERFORM  VARYING   IX   FROM      1    BY   1
                             UNTIL     IX   >    CNT-MEISAI
         ADD  WRK-TEISUU(IX) TO   WRK-TEKEI
*        売上データ作成＝９の時は、照会とする。
         IF   WRK-URISAK(IX)      =    9
              MOVE      1    TO   SYO-FLG
         END-IF
     END-PERFORM.
     MOVE     WRK-HAKEI      TO   HAKEI.
     MOVE     WRK-TEKEI      TO   TEKEI.
*
 GAMEN-SET-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｳﾘｱｹﾞﾃﾞﾝﾋﾟｮｳ ｿﾝｻﾞｲ ﾁｪｯｸ                     *
*--------------------------------------------------------------*
 220-SONZAI-CHECK       SECTION.
     MOVE    "220-SONZAI-CHECK"  TO   S-NAME.
*
     MOVE     SPACE          TO   DEN-REC.
     INITIALIZE                   DEN-REC.
     MOVE     TORICD         TO   DEN-F01.
     MOVE     DENNO          TO   DEN-F02.
     MOVE     ZERO           TO   DEN-F04.
     MOVE     40             TO   DEN-F051.
     START    SHTDENL1  KEY  >=   DEN-F01   DEN-F02
                                  DEN-F04   DEN-F051
                                  DEN-F03
         INVALID   KEY
              MOVE      5    TO   ERR-FLG
              MOVE     "C"        TO   EDIT-CURSOR OF JDATE
              MOVE     "R"        TO   EDIT-OPTION OF JDATE
              MOVE     "R"        TO   EDIT-OPTION OF JTIME
              MOVE     "R"        TO   EDIT-OPTION OF TORICD
              MOVE     "R"        TO   EDIT-OPTION OF DENNO
              GO   TO   220-SONZAI-CHECK-EXIT
         NOT INVALID
              READ      SHTDENL1  NEXT
                AT END
                   MOVE      5    TO   ERR-FLG
                   MOVE     "C"   TO   EDIT-CURSOR OF JDATE
                   MOVE     "R"   TO   EDIT-OPTION OF JDATE
                   MOVE     "R"   TO   EDIT-OPTION OF JTIME
                   MOVE     "R"   TO   EDIT-OPTION OF TORICD
                   MOVE     "R"   TO   EDIT-OPTION OF DENNO
                   GO   TO   220-SONZAI-CHECK-EXIT
                NOT AT END
                   IF ( TORICD    =    DEN-F01  ) AND
                      ( DENNO     =    DEN-F02  ) AND
                      ( ZERO      =    DEN-F04  ) AND
                      ( 40        =    DEN-F051 )
                        CONTINUE
                   ELSE
                        MOVE      5    TO   ERR-FLG
                        MOVE "C"  TO   EDIT-CURSOR OF JDATE
                        MOVE "R"  TO   EDIT-OPTION OF JDATE
                        MOVE "R"  TO   EDIT-OPTION OF JTIME
                        MOVE "R"  TO   EDIT-OPTION OF TORICD
                        MOVE "R"  TO   EDIT-OPTION OF DENNO
                        GO   TO   220-SONZAI-CHECK-EXIT
                   END-IF
              END-READ
     END-START.
 220-SONZAI-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾃﾞｰﾀﾀｲﾋ ｼﾖﾘ                                 *
*--------------------------------------------------------------*
 220-TAIHI-SEC          SECTION.
     MOVE     "220-TAIHI-SEC"     TO   S-NAME.
*
     IF     ( TORICD    =    DEN-F01  ) AND
            ( DENNO     =    DEN-F02  ) AND
            ( ZERO      =    DEN-F04  ) AND
            ( 40        =    DEN-F051 )
*********IF   DEN-F274  NOT =     1
*********     GO   TO   220-TAIHI-010
*********END-IF
         IF ( DEN-F46   =         JDATE ) AND
            ( DEN-F47   =         JTIME )
              ADD       1         TO   IX
              MOVE      DEN-F03   TO   WRK-GYO    (IX)
              MOVE      DEN-F25   TO   WRK-RYOCD  (IX)
              MOVE      DEN-F1421 TO   WRK-SMEI1  (IX)
              MOVE      DEN-F1422 TO   WRK-SMEI2  (IX)
              MOVE      DEN-F1411 TO   WRK-JSHOCD (IX)
              MOVE      DEN-F1412 TO   WRK-HINTAN (IX)
              MOVE      DEN-F50   TO   WRK-HATSUU (IX)
              ADD       DEN-F50   TO   WRK-HAKEI
              MOVE      DEN-F15   TO   WRK-TEISUU (IX)
                                       WRK-MAETEISUU(IX)
              MOVE      DEN-F172  TO   WRK-GENTAN (IX)
              MOVE      DEN-F181  TO   WRK-GKIN   (IX)
              MOVE      DEN-F277  TO   WRK-URISAK (IX)
              IF        IX   =    1
                   MOVE      DEN-F07   TO   WRK-TENPO
*                  店舗マスタ検索
                   MOVE      TORICD    TO   TEN-F52
                   MOVE      DEN-F07   TO   TEN-F011
                   READ      TENMS1
                        INVALID   KEY
                             MOVE      SPACE     TO   WRK-TENPNM
                        NOT INVALID
                             MOVE      TEN-F02   TO   WRK-TENPNM
                   END-READ
                   MOVE      DEN-F12   TO   WRK-BUNRUI
                   MOVE      DEN-F051  TO   WRK-DKU
                   MOVE      DEN-F111  TO   WRK-HYMD
                   MOVE      DEN-F112  TO   WRK-NYMD
              END-IF
         END-IF
     ELSE
              MOVE      IX        TO   CNT-MEISAI
              GO        TO        220-TAIHI-EXIT
     END-IF.
*
 220-TAIHI-010.
*
     READ     SHTDENL1  NEXT
         AT   END
              MOVE      IX        TO   CNT-MEISAI
         NOT AT END
              GO   TO   220-TAIHI-SEC
     END-READ.
*
 220-TAIHI-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾃｲｾｲ ｽｳﾘｮｳ ﾆｭｳﾘｮｸ                           *
*--------------------------------------------------------------*
 220-INP-GRP02          SECTION.
     MOVE     "220-INP-GRP02"     TO   S-NAME.
     MOVE     "GRP002"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
     MOVE     ZERO      TO        ERR-FLG.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
         WHEN PF04
              MOVE      0         TO   GR-NO  WRK-KEY
         WHEN PF06
              PERFORM   CLR-BODY1-RTN
              MOVE      1         TO   GR-NO
              MOVE      SPACE     TO   MEISAI
         WHEN PF11
              PERFORM   CLR-BODY1-RTN
              IF   CNT-PAGE  <=   1
                   PERFORM   220-GRP02-CHECK-SEC
                   IF   ERR-FLG   =    ZERO
                        PERFORM   GAMEN-TAIHI-SEC
                        MOVE      10   TO   ERR-FLG
                   END-IF
              ELSE
                   PERFORM   220-GRP02-CHECK-SEC
                   IF   ERR-FLG   =    ZERO
                        PERFORM   GAMEN-TAIHI-SEC
                        ADD       -1   TO   CNT-PAGE
                        PERFORM   GAMEN-SET-SEC
                   END-IF
              END-IF
         WHEN PF12
              PERFORM   CLR-BODY1-RTN
              IF   CNT-PAGE  =    CNT-MAXPAGE
                   PERFORM   220-GRP02-CHECK-SEC
                   IF   ERR-FLG   =    ZERO
                        PERFORM   GAMEN-TAIHI-SEC
                        PERFORM   GAMEN-SET-SEC
                        MOVE      11   TO   ERR-FLG
                   END-IF
              ELSE
                   PERFORM   220-GRP02-CHECK-SEC
                   IF   ERR-FLG   =    ZERO
                        PERFORM   GAMEN-TAIHI-SEC
                        ADD       1    TO   CNT-PAGE
                        PERFORM   GAMEN-SET-SEC
                   END-IF
              END-IF
         WHEN ENT
              PERFORM   CLR-BODY1-RTN
              PERFORM   220-GRP02-CHECK-SEC
              IF        ERR-FLG   =    ZERO
                        PERFORM   GAMEN-TAIHI-SEC
                        MOVE      9    TO   GR-NO
              END-IF
         WHEN OTHER
              MOVE           1    TO   ERR-FLG
     END-EVALUATE.
*
 220-INP-GRP02-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾒｲｻｲ    ﾁｪｯｸ                                *
*--------------------------------------------------------------*
 220-GRP02-CHECK-SEC    SECTION.
     MOVE     "220-CRP02-CHECK-SEC"    TO   S-NAME.
     MOVE     ZERO      TO        ERR-FLG.
*
     PERFORM  VARYING   IZ   FROM  1  BY  1  UNTIL  IZ  >  6
         IF   RYOCD (IZ)     NOT =     SPACE
              IF   TEISUU(IZ)     IS NUMERIC
                   IF   HATSUU(IZ)     <    TEISUU(IZ)
                        IF   ERR-FLG   =    ZERO
                             MOVE      14   TO   ERR-FLG
                        END-IF
                        MOVE  "C" TO   EDIT-CURSOR OF TEISUU(IZ)
                        MOVE  "R" TO   EDIT-OPTION OF TEISUU(IZ)
                   END-IF
              ELSE
                   IF   ERR-FLG   =    ZERO
                        MOVE      8    TO   ERR-FLG
                   END-IF
                   MOVE     "C"   TO   EDIT-CURSOR OF TEISUU(IZ)
                   MOVE     "R"   TO   EDIT-OPTION OF TEISUU(IZ)
              END-IF
         END-IF
     END-PERFORM.
*
 220-GRP02-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｶﾞﾒﾝ ﾀｲﾋ ｼｮﾘ                                *
*--------------------------------------------------------------*
 GAMEN-TAIHI-SEC        SECTION.
     MOVE     "GAMEN-TAIHI-SEC"   TO   S-NAME.
*
     COMPUTE  IY   =    CNT-PAGE  *    6    -    6.
     PERFORM  VARYING   IZ   FROM  1  BY  1  UNTIL  IZ  >  6
         ADD  1    TO   IY
         IF   GYO(IZ)   IS NUMERIC
              MOVE TEISUU(IZ)     TO   WRK-TEISUU(IY)
              COMPUTE   GKIN(IZ)  =    TEISUU(IZ)     *
                                       GENTAN(IZ)
         ELSE
              MOVE "X"  TO   EDIT-STATUS OF TEISUU(IZ)
         END-IF
     END-PERFORM.
*
*    合計計算
     MOVE     ZERO           TO   WRK-TEKEI.
     PERFORM  VARYING   IX   FROM      1    BY   1
                             UNTIL     IX   >    CNT-MEISAI
         ADD  WRK-TEISUU(IX) TO   WRK-TEKEI
     END-PERFORM.
     MOVE     WRK-TEKEI      TO   TEKEI.
*
 GAMEN-TAIHI-EXIT.
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
         WHEN PF04
              MOVE      0         TO   GR-NO  WRK-KEY
         WHEN PF06
              IF   SYO-FLG   =    1
                   MOVE      0    TO   GR-NO
              ELSE
                   MOVE      2    TO   GR-NO
              END-IF
         WHEN PF11
              IF   CNT-PAGE  <=   1
                   MOVE      10   TO   ERR-FLG
              ELSE
                   ADD       -1   TO   CNT-PAGE
                   PERFORM   GAMEN-SET-SEC
              END-IF
         WHEN PF12
              IF   CNT-PAGE  =    CNT-MAXPAGE
                   MOVE      11   TO   ERR-FLG
              ELSE
                   ADD       1    TO   CNT-PAGE
                   PERFORM   GAMEN-SET-SEC
              END-IF
         WHEN ENT
              MOVE      ZERO      TO   ERR-FLG
              PERFORM   CLR-TAIL-RTN
              IF        KKNN      NOT =    "Y"
                        IF   SYO-FLG   =    1
                             MOVE      0    TO   GR-NO
                        ELSE
                             MOVE      13   TO   ERR-FLG
                        END-IF
              ELSE
                        MOVE      10   TO   GR-NO
              END-IF
         WHEN OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 230-INP-KKNN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｺｳｼﾝ ﾁｪｯｸ ｼｮﾘ                               *
*--------------------------------------------------------------*
 240-UPDATE-SEC           SECTION.
     MOVE     "240-UPDATE-SEC"    TO   S-NAME.
*
     MOVE     ZERO      TO   UPD-FLG.
     PERFORM  VARYING   IX   FROM      1    BY   1
                             UNTIL     IX   >    CNT-MEISAI
         IF   WRK-TEISUU(IX)      NOT =     WRK-MAETEISUU(IX)
              MOVE      1    TO   UPD-FLG
         END-IF
     END-PERFORM.
     IF       UPD-FLG   NOT =     ZERO
              PERFORM   KOUSIN-SEC
     END-IF.
*
     MOVE     0                   TO   GR-NO.
*
 240-UPDATE-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL         ｺｳｼﾝ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 KOUSIN-SEC             SECTION.
     MOVE    "KOUSIN-SEC"         TO   S-NAME.
*
     PERFORM  VARYING   IX   FROM      1    BY   1
                             UNTIL     IX   >    CNT-MEISAI
         MOVE      SPACE          TO   DEN-REC
         INITIALIZE                    DEN-REC
         MOVE      TORICD         TO   DEN-F01
         MOVE      DENNO          TO   DEN-F02
         MOVE      ZERO           TO   DEN-F04
         MOVE      40             TO   DEN-F051
         MOVE      WRK-GYO  (IX)  TO   DEN-F03
         READ      SHTDENL1
              INVALID   KEY
                   DISPLAY  "SSY0401I  SHTDENL1 INV "
                                       UPON CONS
              NOT INVALID
*                  数量、原価金額、売価金額セット
                   IF   WRK-TEISUU(IX) NOT =  WRK-MAETEISUU(IX)
*                       商品変換ＴＢＬチェック*
                        PERFORM   HSHOTBL-READ-SEC
                        IF  HSHOTBL-INV-FLG  NOT =  "INV"
*                           在庫マスタ更新
                            PERFORM   ZAIKO-SEC
                        END-IF
                        IF   KEP-FLG   =    SPACE
                             MOVE      1         TO   DEN-F27D
                        ELSE
                             MOVE      ZERO      TO   DEN-F27D
                        END-IF
                        MOVE WRK-TEISUU(IX)      TO   DEN-F15
                        COMPUTE   DEN-F181  =
                                  DEN-F172  *    WRK-TEISUU(IX)
                        COMPUTE   DEN-F182  =
                                  DEN-F173  *    WRK-TEISUU(IX)
                   END-IF
*                  訂正ＦＬＧ
*## 1999/11/15 NAV T.T START ##* 担当者をセットする必要有り
*******************MOVE      1    TO   DEN-F53
                   IF   WRK-HAKEI  NOT =  WRK-TEKEI
                        MOVE 1    TO   DEN-F53
************************MOVE 90   TO   DEN-F06
                        MOVE 99   TO   DEN-F06
                   ELSE
                        MOVE ZERO TO   DEN-F53
                        MOVE 99   TO   DEN-F06
                   END-IF
*## 1999/11/15 NAV T.T END   ##*
                   REWRITE   DEN-REC
         END-READ
     END-PERFORM.
*
 KOUSIN-010.
*
 KOUSIN-SEC-EXIT.
     EXIT.
****************************************************************
*              在庫引当                                        *
****************************************************************
 ZAIKO-SEC              SECTION.
*
     MOVE   "ZAIKO-SEC"      TO   S-NAME.
     MOVE    SPACE           TO   KEP-FLG.
*商品在庫マスタ存在チェック
     MOVE    DEN-F08         TO   ZAI-F01.
     MOVE    DEN-F1411       TO   ZAI-F021.
     MOVE    DEN-F1412       TO   ZAI-F022.
     MOVE    DEN-F49         TO   ZAI-F03.
     READ    ZZAIMS
             INVALID
             PERFORM   ZAIKO-UPDATE1-SEC
             NOT  INVALID
             PERFORM   ZAIKO-UPDATE2-SEC
     END-READ.
*
 ZAIKO-EXIT.
     EXIT.
****************************************************************
*              在庫引当                                        *
****************************************************************
 ZAIKO-UPDATE1-SEC      SECTION.
*
     MOVE     "ZAIKO-UPDATE1-SEC" TO   S-NAME.
*商品在庫Ｍが未存在の為、在庫マスタ作成
      MOVE      "1"           TO   KEP-FLG.
*商品在庫マスタ初期化
      MOVE      SPACE         TO   ZAI-REC.
      INITIALIZE                   ZAI-REC.
*商品在庫マスタ項目セット
      MOVE      DEN-F08       TO   ZAI-F01.
      MOVE      DEN-F1411     TO   ZAI-F021.
      MOVE      DEN-F1412     TO   ZAI-F022.
      MOVE      DEN-F49       TO   ZAI-F03.
*未出庫数＝未出庫数＋数量
      COMPUTE   ZAI-F13       =    ZAI-F13  +  WRK-TEISUU(IX).
*商品名称マスタ読込み
      PERFORM   HMEIMS-READ-SEC.
*商品名称マスタ存在チェック
      IF  HMEIMS-INV-FLG  =  SPACE
          MOVE  MEI-F031      TO   ZAI-F04
          WRITE ZAI-REC
      END-IF.
*
 ZAIKO-UPDATE1-EXIT.
      EXIT.
****************************************************************
*              在庫引当                                        *
****************************************************************
 ZAIKO-UPDATE2-SEC      SECTION.
*
     MOVE     "ZAIKO-UPDATE2-SEC" TO   S-NAME.
     INITIALIZE    WRK-AREA2.
*
     IF  DEN-F27D      =     1
*        引当済数に数量減算
         COMPUTE  ZAI-F14   =   ZAI-F14  -  WRK-MAETEISUU(IX)
*        未出庫数に数量減算
         COMPUTE  ZAI-F13   =   ZAI-F13  -  WRK-MAETEISUU(IX)
     ELSE
*        未出庫数に数量減算
         COMPUTE  ZAI-F13   =   ZAI-F13  -  WRK-MAETEISUU(IX)
     END-IF.
*
*引当後在庫数チェック
*    現在庫数－引当済数＝引当可能在庫数
     COMPUTE   WRK-ZAI   =   ZAI-F06  -  ZAI-F14.
*    引当可能在庫数－発注数量＝引当後在庫数
     COMPUTE   WRK-HIK   =   WRK-ZAI  -  WRK-TEISUU(IX).
     IF  WRK-HIK  <  0
         MOVE      "1"      TO   KEP-FLG
*        未出庫数に数量加算
         COMPUTE  ZAI-F13   =   ZAI-F13  +  WRK-TEISUU(IX)
*        商品在庫マスタ更新
         REWRITE  ZAI-REC
     ELSE
*        引当済数に数量加算
         COMPUTE  ZAI-F14   =   ZAI-F14  +  WRK-TEISUU(IX)
*        未出庫数に数量加算
         COMPUTE  ZAI-F13   =   ZAI-F13  +  WRK-TEISUU(IX)
*        商品在庫マスタ更新
         REWRITE  ZAI-REC
     END-IF.
*
 ZAIKO-UPDATE2-EXIT.
     EXIT.
****************************************************************
*                商品変換テーブル読込み                        *
****************************************************************
 HSHOTBL-READ-SEC          SECTION.
*
     MOVE      "HSHOTBL-READ-SEC" TO    S-NAME.
*
     MOVE      DEN-F01     TO     SHO-F01.
     MOVE      DEN-F25     TO     SHO-F02.
     READ      HSHOTBL
               INVALID
               MOVE      "INV"    TO    HSHOTBL-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    HSHOTBL-INV-FLG
     END-READ.
*
 HSHOTBL-READ-EXIT.
     EXIT.
****************************************************************
*                商品名称マスタ読込み                          *
****************************************************************
 HMEIMS-READ-SEC           SECTION.
*
     MOVE      "HMEIMS-READ-SEC"  TO    S-NAME.
     MOVE      SPACE       TO     HMEIMS-INV-FLG.
*
     MOVE      SHO-F031    TO     MEI-F011.
     MOVE      SHO-F032    TO     MEI-F012.
     READ      HMEIMS
               INVALID
               MOVE      "INV"    TO    HMEIMS-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    HMEIMS-INV-FLG
     END-READ.
*
 HMEIMS-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  READ                              *
*--------------------------------------------------------------*
 900-DSP-READ           SECTION.
     MOVE     "900-DSP-READ"      TO   S-NAME.
     MOVE     "SCRERE"       TO   DSP-GRP.
     IF       ERR-FLG   =    0
              MOVE      SPACE               TO   MSG
              MOVE      "D"      TO   EDIT-OPTION OF MSG
     ELSE
              MOVE      MSG-TBL (ERR-FLG)   TO   MSG
     END-IF.
     IF       GR-NO     =    1
              MOVE      GUIDE01   TO   GUIDE
     ELSE
         IF   CNT-MEISAI     <=   6
              MOVE      GUIDE02   TO   GUIDE
         ELSE
              IF   CNT-PAGE  <=   1
                   MOVE      GUIDE03   TO   GUIDE
              ELSE
                   MOVE      GUIDE04   TO   GUIDE
              END-IF
         END-IF
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
     WRITE    FSY04011.
 900-DSP-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＨＥＡＤ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-HEAD-RTN           SECTION.
     MOVE     " "            TO   EDIT-CURSOR OF JDATE
                                  EDIT-CURSOR OF JTIME
                                  EDIT-CURSOR OF TORICD
                                  EDIT-CURSOR OF DENNO.
     MOVE     "M"            TO   EDIT-OPTION OF JDATE
                                  EDIT-OPTION OF JTIME
                                  EDIT-OPTION OF TORICD
                                  EDIT-OPTION OF DENNO.
 CLR-HEAD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＢＯＤＹ１属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-BODY1-RTN          SECTION.
*
     PERFORM  VARYING   IX   FROM  1   BY   1    UNTIL  IX > 6
         MOVE     " "   TO   EDIT-CURSOR OF TEISUU(IX)
         MOVE     "M"   TO   EDIT-OPTION OF TEISUU(IX)
     END-PERFORM.
*
 CLR-BODY1-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＴＡＩＬ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-TAIL-RTN           SECTION.
     MOVE     " "        TO   EDIT-CURSOR OF KKNN.
     MOVE     "M"        TO   EDIT-OPTION OF KKNN.
 CLR-TAIL-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
