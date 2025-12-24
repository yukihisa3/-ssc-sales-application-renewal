****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　営利種子ストックＮＯ管理　　　　　*
*    業務名　　　　　　　：　営利種子ストックＮＯ管理　　      *
*    モジュール名　　　　：　出荷ストックＮＯ入力　　　　　　　*
*    作成日／作成者　　　：　2020/07/15 NAV高橋
*    処理概要　　　　　　：　画面より、バッチ_、倉庫コード    *
*                            商品コードを入力し、明細単位に    *
*                            出荷ストックＮＯ、数量内訳を入　  *
*　　　　　　　　　　　　　　力する。　　　　　　　　　　　　　*
*    更新日／更新者　　　：　                                  *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　    *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SST0060I.
 AUTHOR.                NAV.
 DATE-WRITTEN.          15/05/26.
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
*----<<ストックＮＯ管理ファイル>>--*
     SELECT   STNOKRF   ASSIGN    TO   DA-01-VI-STNOKRL3
                        ORGANIZATION   INDEXED
                        ACCESS    MODE DYNAMIC
                        RECORD    KEY  STN-F01   STN-F02
                                       STN-F03   STN-F04
                                       STN-F19   STN-F05
                                       STN-F06   STN-F07
                                       STN-F08
                        FILE STATUS    STNOKRF-ST.
*----<< 伝票データ >>--*
     SELECT   SHTDENF  ASSIGN         DA-01-VI-SHTDENLA
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  DE2-F46   DE2-F47
                                       DE2-F01   DE2-F48
                                       DE2-F02   DE2-F04
                                       DE2-F051  DE2-F07
                                       DE2-F112  DE2-F03
                        WITH  DUPLICATES
                        STATUS         SHTDENF-ST.
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS    ASSIGN         DA-01-VI-TOKMS2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TOK-F01
                        STATUS         HTOKMS-ST.
*----<< 店舗マスタ >>-*
     SELECT   HTENMS    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TEN-F52   TEN-F011
                        FILE      STATUS    HTENMS-ST.
*----<<商品名称マスタ>>-*
     SELECT   HMEIMS    ASSIGN         DA-01-VI-MEIMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  MEI-F011
                                       MEI-F012
                        FILE STATUS    HMEIMS-ST.
*----<< 倉庫マスタ >>--*
     SELECT   ZSOKMS     ASSIGN         DA-01-VI-ZSOKMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SOK-F01
                        STATUS         ZSOKMS-ST.
*----<<商品変換ＴＢＬ>>--*
     SELECT   HSHOTBL    ASSIGN         DA-01-VI-SHOTBL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TBL-F01  TBL-F02
                        STATUS         HSHOTBL-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 表示ファイル >>--*
 FD  DSPFILE            LABEL     RECORD   IS   STANDARD.
     COPY     FST00601  OF        XMDLIB.
*----<<ストックＮＯ管理ファイル>>--*
 FD  STNOKRF           LABEL     RECORD   IS   STANDARD.
     COPY     STNOKRF   OF        XFDLIB
              JOINING   STN       PREFIX.
*----<< 伝票データ >>--*
 FD  SHTDENF           LABEL     RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DE2       PREFIX.
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< 店舗マスタ >>--*
 FD  HTENMS             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*----<< 商品名称マスタ >>--*
 FD  HMEIMS             LABEL RECORD   IS   STANDARD.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
*----<< 倉庫マスタ >>--*
 FD  ZSOKMS            LABEL RECORD   IS   STANDARD.
     COPY     ZSOKMS   OF        XFDLIB
              JOINING   SOK       PREFIX.
*----<<商品変換ＴＢＬ>>--*
 FD  HSHOTBL           LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL  OF        XFDLIB
              JOINING   TBL       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  ERR-FLG             PIC  9(02)  VALUE  ZERO.
     03  CHK-CNT             PIC  9(01)  VALUE  ZERO.
     03  KEP-FLG             PIC  X(01)  VALUE  SPACE.
     03  HTOKMS-INV-FLG      PIC  X(03)  VALUE  SPACE.
     03  HTENMS-INV-FLG      PIC  X(03)  VALUE  SPACE.
     03  HMEIMS-INV-FLG      PIC  X(03)  VALUE  SPACE.
     03  ZSOKMS-INV-FLG      PIC  X(03)  VALUE  SPACE.
     03  HSHOTBL-INV-FLG     PIC  X(03)  VALUE  SPACE.
     03  SHTDENF-INV-FLG     PIC  X(03)  VALUE  SPACE.
     03  CHK-FLG             PIC  X(03)  VALUE  SPACE.
     03  WK-B-HIDUKE         PIC  9(08)  VALUE  ZERO.
     03  WK-B-TIME           PIC  9(04)  VALUE  ZERO.
     03  WK-B-TORICD         PIC  9(08)  VALUE  ZERO.
     03  WK-B-SOKCD          PIC  X(02)  VALUE  SPACE.
     03  WK-B-NOUDT          PIC  9(08)  VALUE  ZERO.
     03  ERK-FLG             PIC  9(01)  VALUE  ZERO.
     03  WK-NOUHIN           PIC  9(08)  VALUE  ZERO.
     03  SAKUBA-CHG-FLG      PIC  X(01)  VALUE  SPACE.
     03  STN-HIT-FLG         PIC  X(01)  VALUE  SPACE.
     03  M-GK-STSU           PIC  9(07)  VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  STNOKRF-ST       PIC  X(02).
 01  SHTDENF-ST       PIC  X(02).
 01  HTOKMS-ST        PIC  X(02).
 01  HTENMS-ST        PIC  X(02).
 01  HMEIMS-ST        PIC  X(02).
 01  ZSOKMS-ST        PIC  X(02).
 01  HSHOTBL-ST       PIC  X(02).
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
 01  GUIDE01       PIC  N(40)  VALUE   NC"_取　消　_終　了".
 01  GUIDE02       PIC  N(40)  VALUE
     NC"_取　消　_終　了　_項目戻り".
 01  GUIDE03       PIC  N(40)  VALUE
     NC"_取　消　_終　了　_項目戻り　_次　頁".
 01  GUIDE04       PIC  N(40)  VALUE
     NC"_取　消　_終　了　_項目戻り　_前　頁".
 01  GUIDE05       PIC  N(40)  VALUE
     NC"_取　消　_終　了　_項目戻り　_前　頁　_次　頁".
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
              NC"ストックＮＯ管理ファイルの存在しません。".
     03  MSG06               PIC  N(30)  VALUE
              NC"取引先コードを入力して下さい。".
     03  MSG07               PIC  N(30)  VALUE
              NC"訂正商品が違います。".
     03  MSG08               PIC  N(30)  VALUE
              NC"倉庫が違います。".
     03  MSG09               PIC  N(30)  VALUE
              NC"訂正商品を入力して下さい。".
     03  MSG10               PIC  N(30)  VALUE
              NC"前頁はありません。".
     03  MSG11               PIC  N(30)  VALUE
              NC"次頁はありません。".
     03  MSG12               PIC  N(30)  VALUE
              NC"内訳数量が未入力です。".
     03  MSG13               PIC  N(30)  VALUE
              NC"Ｙを入力".
     03  MSG14               PIC  N(30)  VALUE
              NC"ストックＮＯが未入力です。".
     03  MSG15               PIC  N(30)  VALUE
         NC"ストックＮＯ１、内訳数量１は必ず入力して下さい。".
     03  MSG16               PIC  N(30)  VALUE
         NC"ストックＮＯ、内訳数量は先頭より詰めて下さい。".
     03  MSG17               PIC  N(30)  VALUE
              NC"納品日に誤りがあります。".
     03  MSG18               PIC  N(30)  VALUE
     NC"ストックＮＯ数量内訳合計と出荷数が一致しません。".
     03  MSG19               PIC  N(30)  VALUE
     NC"欠品制御区分には空白か１を入力して下さい。".
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
 01  FILLER                  REDEFINES   MSG-AREA.
     03  MSG-TBL             PIC  N(30)  OCCURS      25.
*
 01  WRK-AREA.
     03  WRK-MEISAI          OCCURS    999.
       05  WRK-TENPO         PIC  9(05).
       05  WRK-TENPONM       PIC  N(15).
       05  WRK-DENNO         PIC  9(09).
       05  WRK-GYONO         PIC  9(02).
       05  WRK-NOUDT         PIC  9(08).
       05  WRK-HATYUSU       PIC S9(07).
       05  WRK-SYUKASU       PIC S9(07).
       05  WRK-STNO1         PIC  X(06).
       05  WRK-STSU1         PIC  9(05).
       05  WRK-STNO2         PIC  X(06).
       05  WRK-STSU2         PIC  9(05).
       05  WRK-STNO3         PIC  X(06).
       05  WRK-STSU3         PIC  9(05).
       05  WRK-STNO4         PIC  X(06).
       05  WRK-STSU4         PIC  9(05).
       05  WRK-STNO5         PIC  X(06).
       05  WRK-STSU5         PIC  9(05).
     03  WRK-HSUKEI          PIC S9(07)V99.
     03  WRK-TSUKEI          PIC S9(07)V99.
     03  WRK-JTSTNO          PIC  X(06).
 01  WRK-NAME.
     03  WRK-KANJI1          PIC  N(15)        VALUE SPACE.
     03  WRK-KANJI2          PIC  N(15)        VALUE SPACE.
*
*計算領域
 01  WRK-AREA2.
     03  WRK-HIK             PIC S9(09)V9(02)  VALUE ZERO.
     03  WRK-ZAI             PIC S9(09)V9(02)  VALUE ZERO.
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
*明細数量集計
 01  WK-GK-SYUKA.
     03  S-STSU1             PIC  9(07)  VALUE  ZERO.
     03  S-STSU2             PIC  9(07)  VALUE  ZERO.
     03  S-STSU3             PIC  9(07)  VALUE  ZERO.
     03  S-STSU4             PIC  9(07)  VALUE  ZERO.
     03  S-STSU5             PIC  9(07)  VALUE  ZERO.
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
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SST0060I DSPFILE ERROR " DSP-CNTL " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     STOP     RUN.
*----<< 伝票データ >>--*
 STNOKRF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      STNOKRF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SST0060I STNOKRL1 ERROR " STNOKRF-ST  " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     STOP     RUN.
*----<< 伝票データ >>--*
 SHTDENF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHTDENF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SST0060I SHTDENLA ERROR " SHTDENF-ST  " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     STOP     RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SST0060I HTOKMS ERROR " HTOKMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     STOP     RUN.
*----<< 店舗マスタ >>--*
 HTENMS-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTENMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SST0060I HTENMS ERROR " HTENMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     STOP     RUN.
*----<< 商品名称マスタ >>--*
 HMEIMS-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HMEIMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SST0060I HMEIMS ERROR " HMEIMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     STOP     RUN.
*----<< 倉庫マスタ >>--*
 ZSOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      ZSOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SST0060I ZSOKMS ERROR " ZSOKMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     STOP     RUN.
*----<<商品変換ＴＢＬ>>--*
 HSHOTBL-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HSHOTBL.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SST0060I SHOTBL1 ERROR " HSHOTBL-ST " "
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
     MOVE     SYS-TIME       TO   SYS-TIME2.
     DISPLAY  "*** SST0060I START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     I-O       DSPFILE.
     OPEN     INPUT     SHTDENF.
     OPEN     INPUT     HTOKMS.
     OPEN     INPUT     HTENMS.
     OPEN     INPUT     HMEIMS.
     OPEN     INPUT     ZSOKMS.
     OPEN     INPUT     HSHOTBL.
     OPEN     I-O       STNOKRF.
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
*----<< ﾊﾞｯﾁNO. ﾆｭｳﾘｮｸ >>-*
     PERFORM  220-INP-GRP01  UNTIL     GR-NO    NOT  =    1.
*----<< ﾒｲｻｲ    ﾆｭｳﾘｮｸ >>-*
     PERFORM  220-INP-GRP02  UNTIL     GR-NO    NOT  =    2.
*----<< ﾒｲｻｲ    ﾆｭｳﾘｮｸ >>-*
     PERFORM  220-INP-GRP03  UNTIL     GR-NO    NOT  =    3.
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
     CLOSE    STNOKRF.
     CLOSE    SHTDENF.
     CLOSE    HTOKMS.
     CLOSE    HTENMS.
     CLOSE    HMEIMS.
     CLOSE    ZSOKMS.
     CLOSE    HSHOTBL.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SST0060I END   *** "
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
     MOVE     SPACE          TO   FST00601.
*
     PERFORM  CLR-HEAD-RTN.
     PERFORM  CLR-BODY1-RTN.
     PERFORM  CLR-TAIL-RTN.
*
     MOVE    "SST0060I"      TO   PGID.
     MOVE    "FST00601"      TO   FORM.
*
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     "FST00601"     TO   DSP-FMT.
     MOVE     "SCREFX"       TO   DSP-GRP.
     PERFORM  900-DSP-WRITE.
*毎回バッチ番号を入力させない。
     IF        CHK-FLG  =  SPACE
               MOVE   " "    TO   EDIT-STATUS OF JDATE
               MOVE   " "    TO   EDIT-STATUS OF JTIME
               MOVE   " "    TO   EDIT-STATUS OF TORICD
               MOVE   " "    TO   EDIT-STATUS OF SOKO
     ELSE
               MOVE   "X"    TO   EDIT-STATUS OF JDATE
               MOVE   "X"    TO   EDIT-STATUS OF JTIME
               MOVE   "X"    TO   EDIT-STATUS OF TORICD
               MOVE   "X"    TO   EDIT-STATUS OF SOKO
               MOVE WK-B-HIDUKE TO JDATE
               MOVE WK-B-TIME   TO JTIME
               MOVE WK-B-TORICD TO TORICD
               MOVE WK-B-SOKCD  TO SOKO
               MOVE WK-B-NOUDT  TO NOUHIN
     END-IF.
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
              MOVE      0         TO   GR-NO
              MOVE      SPACE     TO   CHK-FLG
         WHEN PF05
              MOVE      99        TO   GR-NO
         WHEN ENT
              INITIALIZE               WRK-AREA
              INITIALIZE               CNT-AREA
              PERFORM   CLR-HEAD-RTN
              PERFORM   CLR-BODY1-RTN
              PERFORM   220-GRP01-CHECK-SEC
              IF        ERR-FLG   =    ZERO
***                     画面セット処理
                        MOVE      1    TO   CNT-PAGE
                        PERFORM   GAMEN-SET-SEC
************************訂正数計を表示しておく2015/06/15
                        MOVE WRK-TSUKEI TO  JTUNOU
                        MOVE      2    TO   GR-NO
              END-IF
         WHEN OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 220-INP-GRP01-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ヘッド部チェック                            *
*--------------------------------------------------------------*
 220-GRP01-CHECK-SEC    SECTION.
     MOVE     "220-CRP01-CHECK-SEC"    TO   S-NAME.
     MOVE     ZERO      TO        ERR-FLG.
     MOVE     SPACE     TO        HMEIMS-INV-FLG.
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
                   IF   ERR-FLG   =    ZERO
                        MOVE      4    TO   ERR-FLG
                   END-IF
                   MOVE     "C"   TO   EDIT-CURSOR OF TORICD
                   MOVE     "R"   TO   EDIT-OPTION OF TORICD
         END-READ
     ELSE
         IF   ERR-FLG   =    ZERO
              MOVE      6    TO   ERR-FLG
         END-IF
         MOVE     "C"   TO   EDIT-CURSOR OF TORICD
         MOVE     "R"   TO   EDIT-OPTION OF TORICD
     END-IF.
*    倉庫コードチェック
     IF       SOKO      NOT =     SPACE
              MOVE      SOKO      TO   SOK-F01
              READ      ZSOKMS
                  INVALID  KEY
                        MOVE      SPACE     TO   SOKONM
                        IF   ERR-FLG   =    ZERO
                             MOVE      8    TO   ERR-FLG
                        END-IF
                        MOVE     "C"   TO   EDIT-CURSOR OF SOKO
                        MOVE     "R"   TO   EDIT-OPTION OF SOKO
                  NOT INVALID  KEY
                        MOVE      SOK-F02   TO   SOKONM
              END-READ
     ELSE
              MOVE      SPACE     TO   SOKONM
              IF        ERR-FLG   =    ZERO
                        MOVE      8    TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF SOKO
              MOVE     "R"   TO   EDIT-OPTION OF SOKO
     END-IF.
*    納品日チェック
     IF     ( NOUHIN    IS NUMERIC     ) AND
            ( NOUHIN    NOT =     ZERO )
              MOVE     "2"        TO        LINK-IN-KBN
              MOVE      NOUHIN    TO        LINK-IN-YMD8
              CALL     "SKYDTCKB" USING     LINK-IN-KBN
                                            LINK-IN-YMD6
                                            LINK-IN-YMD8
                                            LINK-OUT-RET
                                            LINK-OUT-YMD8
              IF   LINK-OUT-RET   NOT =     ZERO
                   IF   ERR-FLG   =    ZERO
                        MOVE      17   TO   ERR-FLG
                   END-IF
                   MOVE     "C"   TO   EDIT-CURSOR OF NOUHIN
                   MOVE     "R"   TO   EDIT-OPTION OF NOUHIN
              END-IF
     END-IF.
*    相手商品コードチェック
     IF  JANCD     NOT =     SPACE
*        商品変換テーブル検索
         MOVE      SPACE     TO        TBL-REC
         INITIALIZE                    TBL-REC
         MOVE      TORICD    TO        TBL-F01
         MOVE      JANCD     TO        TBL-F02
         READ      HSHOTBL
             INVALID
                   MOVE      SPACE     TO   SYONM1
                   IF   ERR-FLG   =    ZERO
                        MOVE      7    TO   ERR-FLG
                   END-IF
                   MOVE     "C"   TO   EDIT-CURSOR OF JANCD
                   MOVE     "R"   TO   EDIT-OPTION OF JANCD
             NOT INVALID
*                  商品名称マスタ検索
                   MOVE      SPACE     TO   MEI-REC
                   INITIALIZE               MEI-REC
                   MOVE      TBL-F031  TO   MEI-F011
                   MOVE      TBL-F032  TO   MEI-F012
                   READ      HMEIMS
                        INVALID
                        MOVE "INV"     TO   HMEIMS-INV-FLG
                        MOVE ALL NC"＊" TO  SYONM1
                        NOT INVALID
                        MOVE MEI-F021        TO   WRK-KANJI1
                        MOVE MEI-F022        TO   WRK-KANJI2
************************MOVE MEI-F021        TO   SYONM1(1:15)
                        MOVE WRK-NAME        TO   SYONM1
************************MOVE MEI-F022(1:11)  TO   SYONM1(16:11)
                   END-READ
         END-READ
     ELSE
         MOVE      SPACE     TO   SYONM1
         IF   ERR-FLG   =    ZERO
              MOVE      9    TO   ERR-FLG
         END-IF
         MOVE     "C"   TO   EDIT-CURSOR OF JANCD
         MOVE     "R"   TO   EDIT-OPTION OF JANCD
     END-IF.
*    店舗ＣＤチェック
     IF  HTENCD    IS NUMERIC     AND
         HTENCD    NOT =     ZERO
         MOVE      SPACE     TO   TEN-REC
         INITIALIZE               TEN-REC
         MOVE      TORICD    TO   TEN-F52
         MOVE      HTENCD    TO   TEN-F011
         READ      HTENMS
             INVALID
                   IF   ERR-FLG   =    ZERO
                        MOVE      4    TO   ERR-FLG
                   END-IF
                   MOVE     "C"   TO   EDIT-CURSOR OF HTENCD
                   MOVE     "R"   TO   EDIT-OPTION OF HTENCD
             NOT INVALID  KEY
                   MOVE  TEN-F02  TO   HTENNM
         END-READ
     ELSE
         MOVE      SPACE          TO   HTENNM
     END-IF.
*    ストックＮＯ管理ファイルチェック
     IF  ERR-FLG        =    ZERO
         PERFORM   220-SONZAI-CHECK
         IF   ERR-FLG   =    ZERO
              MOVE      ZERO      TO   IX
              PERFORM   220-TAIHI-SEC
              IF        CNT-MEISAI     <=   5
                   MOVE      1         TO   CNT-MAXPAGE
              ELSE
                   DIVIDE    CNT-MEISAI     BY   5
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
*発注数合計／出荷数合計
     MOVE     WRK-HSUKEI          TO   HACSUU  HSUKEI.
     MOVE     WRK-TSUKEI          TO   JTUNOU  TSUKEI.
*
     COMPUTE  IY   =    CNT-PAGE  *    5   -    5.
     PERFORM  VARYING   IZ   FROM  1   BY   1    UNTIL IZ > 5
         COMPUTE   IX   =    IY   +    IZ
         IF ( WRK-TENPO(IX)  NOT =     ZERO ) AND
            ( WRK-DENNO(IX)  NOT =     ZERO )
              MOVE      SPACE          TO   MAS001(IZ)
              MOVE      WRK-TENPO(IX)  TO   TENPO(IZ)
              MOVE      TORICD         TO   TEN-F52
              MOVE      WRK-TENPO(IX)  TO   TEN-F011
              READ      HTENMS
                   INVALID   KEY
                        MOVE      SPACE     TO   TENPNM(IZ)
                   NOT INVALID
                        MOVE      TEN-F03   TO   TENPNM(IZ)
              END-READ
              MOVE WRK-DENNO(IX)       TO   DENNO(IZ)
              MOVE WRK-NOUDT(IX)       TO   NOHIN(IZ)
              MOVE WRK-HATYUSU(IX)     TO   HSUU (IZ)
              MOVE WRK-SYUKASU(IX)     TO   TEISUU(IZ)
              IF   WRK-STNO1(IX) NOT =  SPACE
                   MOVE WRK-STNO1(IX)  TO   MSTNO1(IZ)
                   MOVE WRK-STSU1(IX)  TO   MSTSU1(IZ)
              END-IF
              IF   WRK-STNO2(IX) NOT =  SPACE
                   MOVE WRK-STNO2(IX)  TO   MSTNO2(IZ)
                   MOVE WRK-STSU2(IX)  TO   MSTSU2(IZ)
              END-IF
              IF   WRK-STNO3(IX) NOT =  SPACE
                   MOVE WRK-STNO3(IX)  TO   MSTNO3(IZ)
                   MOVE WRK-STSU3(IX)  TO   MSTSU3(IZ)
              END-IF
              IF   WRK-STNO4(IX) NOT =  SPACE
                   MOVE WRK-STNO4(IX)  TO   MSTNO4(IZ)
                   MOVE WRK-STSU4(IX)  TO   MSTSU4(IZ)
              END-IF
              IF   WRK-STNO5(IX) NOT =  SPACE
                   MOVE WRK-STNO5(IX)  TO   MSTNO5(IZ)
                   MOVE WRK-STSU5(IX)  TO   MSTSU5(IZ)
              END-IF
         ELSE
              MOVE      SPACE          TO   MAS001(IZ)
              MOVE      "X"  TO   EDIT-STATUS OF MSTNO1(IZ)
              MOVE      "X"  TO   EDIT-STATUS OF MSTSU1(IZ)
              MOVE      "X"  TO   EDIT-STATUS OF MSTNO2(IZ)
              MOVE      "X"  TO   EDIT-STATUS OF MSTSU2(IZ)
              MOVE      "X"  TO   EDIT-STATUS OF MSTNO3(IZ)
              MOVE      "X"  TO   EDIT-STATUS OF MSTSU3(IZ)
              MOVE      "X"  TO   EDIT-STATUS OF MSTNO4(IZ)
              MOVE      "X"  TO   EDIT-STATUS OF MSTSU4(IZ)
              MOVE      "X"  TO   EDIT-STATUS OF MSTNO5(IZ)
              MOVE      "X"  TO   EDIT-STATUS OF MSTSU5(IZ)
         END-IF
     END-PERFORM.
*
 GAMEN-SET-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｳﾘｱｹﾞﾃﾞﾝﾋﾟｮｳ ｿﾝｻﾞｲ ﾁｪｯｸ                     *
*--------------------------------------------------------------*
 220-SONZAI-CHECK       SECTION.
     MOVE    "220-SONZAI-CHECK"  TO   S-NAME.
*
     MOVE     SPACE          TO   STN-REC.
     INITIALIZE                   STN-REC.
     MOVE     JDATE          TO   STN-F01.
     MOVE     JTIME          TO   STN-F02.
     MOVE     TORICD         TO   STN-F03.
     MOVE     SOKO           TO   STN-F04.
     MOVE     JANCD          TO   STN-F19.
     MOVE     NOUHIN         TO   STN-F05.
     MOVE     HTENCD         TO   STN-F06.
     START    STNOKRF  KEY  >=    STN-F01   STN-F02   STN-F03
                                  STN-F04   STN-F19   STN-F05
                                  STN-F06   STN-F07   STN-F08
         INVALID   KEY
******   DISPLAY "AAA" UPON CONS
         MOVE      5    TO   ERR-FLG
         MOVE     "C"        TO   EDIT-CURSOR OF JDATE
         MOVE     "R"        TO   EDIT-OPTION OF JDATE
         MOVE     "R"        TO   EDIT-OPTION OF JTIME
         MOVE     "R"        TO   EDIT-OPTION OF TORICD
         MOVE     "R"        TO   EDIT-OPTION OF SOKO
         MOVE     "R"        TO   EDIT-OPTION OF JANCD
         MOVE     "R"        TO   EDIT-OPTION OF NOUHIN
         MOVE     "R"        TO   EDIT-OPTION OF HTENCD
         GO   TO   220-SONZAI-CHECK-EXIT
     NOT INVALID
         READ      STNOKRF  NEXT
           AT END
*****    DISPLAY "BBB" UPON CONS
              MOVE      5    TO   ERR-FLG
              MOVE     "C"   TO   EDIT-CURSOR OF JDATE
              MOVE     "R"   TO   EDIT-OPTION OF JDATE
              MOVE     "R"   TO   EDIT-OPTION OF JTIME
              MOVE     "R"   TO   EDIT-OPTION OF TORICD
              MOVE     "R"   TO   EDIT-OPTION OF SOKO
              MOVE     "R"   TO   EDIT-OPTION OF JANCD
              MOVE     "R"   TO   EDIT-OPTION OF NOUHIN
              MOVE     "R"   TO   EDIT-OPTION OF HTENCD
              GO   TO   220-SONZAI-CHECK-EXIT
           NOT AT END
              IF ( JDATE     =    STN-F01 ) AND
                 ( JTIME     =    STN-F02 ) AND
                 ( TORICD    =    STN-F03 ) AND
                 ( SOKO      =    STN-F04 ) AND
                 ( JANCD     =    STN-F19 )
                 IF  NOUHIN  NOT = ZERO
                     IF  NOUHIN  = STN-F05
                         CONTINUE
                     ELSE
*****    DISPLAY "CCC" UPON CONS
                         MOVE      5    TO   ERR-FLG
                         MOVE "C"  TO   EDIT-CURSOR OF JDATE
                         MOVE "R"  TO   EDIT-OPTION OF JDATE
                         MOVE "R"  TO   EDIT-OPTION OF JTIME
                         MOVE "R"  TO   EDIT-OPTION OF TORICD
                         MOVE "R"  TO   EDIT-OPTION OF SOKO
                         MOVE "R"  TO   EDIT-OPTION OF JANCD
                         MOVE "R"  TO   EDIT-OPTION OF NOUHIN
                         MOVE "R"  TO   EDIT-OPTION OF HTENCD
                         GO   TO   220-SONZAI-CHECK-EXIT
                     END-IF
                 END-IF
                 IF  HTENCD  NOT = ZERO
                     IF  HTENCD  = STN-F06
                         CONTINUE
                     ELSE
*****    DISPLAY "DDD" UPON CONS
                         MOVE      5    TO   ERR-FLG
                         MOVE "C"  TO   EDIT-CURSOR OF JDATE
                         MOVE "R"  TO   EDIT-OPTION OF JDATE
                         MOVE "R"  TO   EDIT-OPTION OF JTIME
                         MOVE "R"  TO   EDIT-OPTION OF TORICD
                         MOVE "R"  TO   EDIT-OPTION OF SOKO
                         MOVE "R"  TO   EDIT-OPTION OF JANCD
                         MOVE "R"  TO   EDIT-OPTION OF NOUHIN
                         MOVE "R"  TO   EDIT-OPTION OF HTENCD
                         GO   TO   220-SONZAI-CHECK-EXIT
                     END-IF
                 END-IF
              ELSE
*****    DISPLAY "EEE" UPON CONS
                   MOVE      5    TO   ERR-FLG
                   MOVE "C"  TO   EDIT-CURSOR OF JDATE
                   MOVE "R"  TO   EDIT-OPTION OF JDATE
                   MOVE "R"  TO   EDIT-OPTION OF JTIME
                   MOVE "R"  TO   EDIT-OPTION OF TORICD
                   MOVE "R"  TO   EDIT-OPTION OF SOKO
                   MOVE "R"  TO   EDIT-OPTION OF JANCD
                   MOVE "R"  TO   EDIT-OPTION OF NOUHIN
                   MOVE "R"  TO   EDIT-OPTION OF HTENCD
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
*
     IF     ( JDATE     =    STN-F01 ) AND
            ( JTIME     =    STN-F02 ) AND
            ( TORICD    =    STN-F03 ) AND
            ( SOKO      =    STN-F04 ) AND
            ( JANCD     =    STN-F19 )
              IF   NOUHIN         NOT =     ZERO
                   IF   NOUHIN         =    STN-F05
                        CONTINUE
                   ELSE
                        GO   TO   220-TAIHI-010
                   END-IF
              END-IF
              IF   HTENCD         NOT =     ZERO
              AND  HTENCD         NUMERIC
                   IF   HTENCD         =    STN-F06
                        CONTINUE
                   ELSE
                        GO   TO   220-TAIHI-010
                   END-IF
              END-IF
              ADD       1         TO   IX
              MOVE      STN-F06   TO   WRK-TENPO  (IX)
**************MOVE      TORICD    TO   TEN-F52
*             MOVE      STN-F07   TO   TEN-F011
*             READ      HTENMS
*                       INVALID
*                       MOVE ALL NC"＊"  TO  WRK-TENPONM(IX)
*                       NOT  INVALID
*                       MOVE TEN-F02     TO  WRK-TENPONM(IX)
**************END-READ
              MOVE      SPACE     TO   WRK-TENPONM(IX)
              MOVE      STN-F07   TO   WRK-DENNO  (IX)
              MOVE      STN-F08   TO   WRK-GYONO  (IX)
              MOVE      STN-F05   TO   WRK-NOUDT  (IX)
              MOVE      STN-F13   TO   WRK-HATYUSU(IX)
              PERFORM  SHTDENF-READ-SEC
              IF  SHTDENF-INV-FLG  = "INV"
                   MOVE      STN-F14   TO   WRK-SYUKASU(IX)
              ELSE
                   MOVE      DE2-F15   TO   WRK-SYUKASU(IX)
              END-IF
              MOVE      STN-F30   TO   WRK-STNO1  (IX)
              MOVE      STN-F31   TO   WRK-STSU1  (IX)
              MOVE      STN-F32   TO   WRK-STNO2  (IX)
              MOVE      STN-F33   TO   WRK-STSU2  (IX)
              MOVE      STN-F34   TO   WRK-STNO3  (IX)
              MOVE      STN-F35   TO   WRK-STSU3  (IX)
              MOVE      STN-F36   TO   WRK-STNO4  (IX)
              MOVE      STN-F37   TO   WRK-STSU4  (IX)
              MOVE      STN-F38   TO   WRK-STNO5  (IX)
              MOVE      STN-F39   TO   WRK-STSU5  (IX)
              ADD       STN-F13   TO   WRK-HSUKEI
              ADD       STN-F14   TO   WRK-TSUKEI
              GO        TO        220-TAIHI-010
     ELSE
              MOVE      IX        TO   CNT-MEISAI
              GO        TO        220-TAIHI-EXIT
     END-IF.
*
 220-TAIHI-010.
*
     READ     STNOKRF   NEXT
         AT   END
              MOVE      IX        TO   CNT-MEISAI
         NOT AT END
              GO   TO   220-TAIHI-SEC
     END-READ.
*
 220-TAIHI-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      一括変更部　入力　                          *
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
              MOVE      0         TO   GR-NO
              MOVE      SPACE     TO   CHK-FLG
         WHEN PF06
              PERFORM   CLR-BODY1-RTN
              MOVE      1         TO   GR-NO
              PERFORM   VARYING   IZ   FROM  1  BY  1
                                       UNTIL    IZ  >  5
                   MOVE      SPACE     TO   MAS001(IZ)
              END-PERFORM
         WHEN ENT
              PERFORM   CLR-BODY1-RTN
              MOVE      DSTNO     TO   WRK-JTSTNO
              IF        ERR-FLG   =    ZERO
                        MOVE      3    TO   GR-NO
              END-IF
         WHEN OTHER
              MOVE           1    TO   ERR-FLG
     END-EVALUATE.
*
 220-INP-GRP02-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      一括変更部　チェック                        *
*--------------------------------------------------------------*
 220-GRP02-CHECK-SEC    SECTION.
     MOVE     "220-CRP02-CHECK-SEC"    TO   S-NAME.
     MOVE     ZERO      TO        ERR-FLG.
*
*
 220-GRP02-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      明細（訂正数・作場）入力                    *
*--------------------------------------------------------------*
 220-INP-GRP03          SECTION.
     MOVE     "220-INP-GRP03"     TO   S-NAME.
     MOVE      ZERO      TO   WRK-TSUKEI
     PERFORM   VARYING   IX   FROM  1  BY  1
                         UNTIL     IX   >  CNT-MEISAI
*      実納品数ゼロ時，訂正後発注数をオールゼロ表示
         IF    DSTNO   NOT = SPACE
         AND   ERR-FLG  =    ZERO
               MOVE DSTNO           TO   WRK-STNO1(IX)
               MOVE WRK-SYUKASU(IX) TO   WRK-STSU1(IX)
               MOVE SPACE           TO   WRK-STNO2(IX)
               MOVE ZERO            TO   WRK-STSU2(IX)
               MOVE SPACE           TO   WRK-STNO3(IX)
               MOVE ZERO            TO   WRK-STSU3(IX)
               MOVE SPACE           TO   WRK-STNO4(IX)
               MOVE ZERO            TO   WRK-STSU4(IX)
               MOVE SPACE           TO   WRK-STNO5(IX)
               MOVE ZERO            TO   WRK-STSU5(IX)
         END-IF
         ADD   WRK-SYUKASU(IX)      TO    WRK-TSUKEI
*
     END-PERFORM.
*
     PERFORM   GAMEN-SET-SEC.
     MOVE     "GRP003"       TO   WK-GRP.
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
              PERFORM   CLR-BODY1-RTN
              MOVE      2         TO   GR-NO
         WHEN PF11
              PERFORM 900-DSP-WRITE2
              PERFORM   MEISAI-CHK-SEC
              IF   ERR-FLG  =  ZERO
                   PERFORM   CLR-BODY1-RTN
                   IF   CNT-PAGE  <=   1
                        PERFORM   GAMEN-TAIHI-SEC
                        MOVE      10   TO   ERR-FLG
                   ELSE
                        PERFORM   GAMEN-TAIHI-SEC
                        IF   ERR-FLG   =    ZERO
                             ADD       -1   TO   CNT-PAGE
                             PERFORM   GAMEN-SET-SEC
                        END-IF
                   END-IF
              END-IF
         WHEN PF12
              PERFORM 900-DSP-WRITE2
              PERFORM   MEISAI-CHK-SEC
              IF   ERR-FLG  =  ZERO
                   PERFORM   CLR-BODY1-RTN
                   IF   CNT-PAGE  =    CNT-MAXPAGE
                        PERFORM   GAMEN-TAIHI-SEC
                        MOVE      11   TO   ERR-FLG
                   ELSE
                        PERFORM   GAMEN-TAIHI-SEC
                        IF   ERR-FLG   =    ZERO
                             ADD       1    TO   CNT-PAGE
                             PERFORM   GAMEN-SET-SEC
                        END-IF
                   END-IF
              END-IF
         WHEN ENT
              PERFORM   GAMEN-TAIHI-SEC
              PERFORM   220-GRP03-CHECK-SEC
              IF        ERR-FLG   =    ZERO
                        MOVE      9    TO   GR-NO
                        PERFORM   CLR-BODY1-RTN
              END-IF
         WHEN OTHER
              MOVE           1    TO   ERR-FLG
     END-EVALUATE.
*
 220-INP-GRP03-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      明細（訂正数・作場）チェック                *
*--------------------------------------------------------------*
 220-GRP03-CHECK-SEC    SECTION.
     MOVE     "220-CRP03-CHECK-SEC"    TO   S-NAME.
     MOVE     ZERO      TO        ERR-FLG.
     PERFORM  VARYING   IZ   FROM  1  BY  1  UNTIL  IZ  >  5
       MOVE   ZERO                 TO    CHK-CNT
      IF  TENPO(IZ)  NOT =  ZERO
      AND TENPO(IZ)  NUMERIC
*ストックＮＯ１
       IF  MSTNO1(IZ)  NOT =  SPACE   *>　ＳＴ１　入力
           IF  MSTSU1(IZ)  =  ZERO    *>  数量１　０
                IF   ERR-FLG   =    ZERO
*****  DISPLAY "1-12" UPON CONS
                     MOVE      12   TO   ERR-FLG
                END-IF
           ELSE                   *>  数量１　入力　正常
                ADD       1    TO   CHK-CNT
           END-IF
       ELSE                       *>　ＳＴ１　空白
           IF  MSTSU1(IZ) NOT = ZERO  *>  数量１　０以外
           AND MSTSU1(IZ) NUMERIC
                IF   ERR-FLG   =    ZERO
*****  DISPLAY "1-14" UPON CONS
                     MOVE      14   TO   ERR-FLG
                END-IF
            ELSE                  *>  数量１　０
                IF   ERR-FLG   =    ZERO
*****  DISPLAY "1-15 = " IZ  UPON CONS
                     MOVE      15   TO   ERR-FLG
                END-IF
           END-IF
       END-IF
*ストックＮＯ２
       IF  MSTNO2(IZ)  NOT =  SPACE   *>　ＳＴ２　入力
           IF  MSTSU2(IZ)  =  ZERO    *>  数量２　０
                IF   ERR-FLG   =    ZERO
*****  DISPLAY "2-12" UPON CONS
                     MOVE      12   TO   ERR-FLG
                END-IF
           ELSE                   *>  数量２　入力　正常
                ADD       1    TO   CHK-CNT
                IF  CHK-CNT NOT = 2
                    IF   ERR-FLG   =    ZERO
*****  DISPLAY "2-16" UPON CONS
                         MOVE      16   TO   ERR-FLG
                    END-IF
                END-IF
           END-IF
       ELSE                       *>　ＳＴ２　空白
           IF  MSTSU2(IZ) NOT = ZERO  *>  数量１２０以外
           AND MSTSU2(IZ) NUMERIC
                IF   ERR-FLG   =    ZERO
*****  DISPLAY "2-14" UPON CONS
                     MOVE      14   TO   ERR-FLG
                END-IF
           END-IF
       END-IF
*ストックＮＯ３
       IF  MSTNO3(IZ)  NOT =  SPACE   *>　ＳＴ３　入力
           IF  MSTSU3(IZ)  =  ZERO    *>  数量３　０
                IF   ERR-FLG   =    ZERO
*****  DISPLAY "3-12" UPON CONS
                     MOVE      12   TO   ERR-FLG
                END-IF
           ELSE                   *>  数量３　入力　正常
                ADD       1    TO   CHK-CNT
                IF  CHK-CNT NOT = 3
                    IF   ERR-FLG   =    ZERO
*****  DISPLAY "3-16" UPON CONS
                         MOVE      16   TO   ERR-FLG
                    END-IF
                END-IF
           END-IF
       ELSE                       *>　ＳＴ３　空白
           IF  MSTSU3(IZ) NOT = ZERO  *>  数量３　０以外
           AND MSTSU3(IZ) NUMERIC
                IF   ERR-FLG   =    ZERO
*****  DISPLAY "3-14" UPON CONS
                     MOVE      14   TO   ERR-FLG
                END-IF
           END-IF
       END-IF
*ストックＮＯ４
       IF  MSTNO4(IZ)  NOT =  SPACE   *>　ＳＴ４　入力
           IF  MSTSU4(IZ)  =  ZERO    *>  数量４　０
                IF   ERR-FLG   =    ZERO
*****  DISPLAY "4-12" UPON CONS
                     MOVE      12   TO   ERR-FLG
                END-IF
           ELSE                   *>  数量４　入力　正常
                ADD       1    TO   CHK-CNT
                IF  CHK-CNT NOT = 4
                    IF   ERR-FLG   =    ZERO
*****  DISPLAY "4-16" UPON CONS
                         MOVE      16   TO   ERR-FLG
                    END-IF
                END-IF
           END-IF
       ELSE                       *>　ＳＴ２　空白
           IF  MSTSU4(IZ) NOT = ZERO  *>  数量１２０以外
           AND MSTSU4(IZ) NUMERIC
                IF   ERR-FLG   =    ZERO
*****  DISPLAY "4-14" UPON CONS
                     MOVE      14   TO   ERR-FLG
                END-IF
           END-IF
       END-IF
*ストックＮＯ５
       IF  MSTNO5(IZ)  NOT =  SPACE   *>　ＳＴ５　入力
           IF  MSTSU5(IZ)  =  ZERO    *>  数量５　０
                IF   ERR-FLG   =    ZERO
*****  DISPLAY "5-12" UPON CONS
                     MOVE      12   TO   ERR-FLG
                END-IF
           ELSE                   *>  数量５　入力　正常
                ADD       1    TO   CHK-CNT
                IF  CHK-CNT NOT = 5
                    IF   ERR-FLG   =    ZERO
*****  DISPLAY "5-16" UPON CONS
                         MOVE      16   TO   ERR-FLG
                    END-IF
                END-IF
           END-IF
       ELSE                       *>　ＳＴ５　空白
           IF  MSTSU5(IZ) NOT = ZERO  *>  数量５　０以外
           AND MSTSU5(IZ) NUMERIC
                IF   ERR-FLG   =    ZERO
*****  DISPLAY "5-14" UPON CONS
                     MOVE      14   TO   ERR-FLG
                END-IF
           END-IF
       END-IF
*出荷数＝数量内訳合計チェック
       IF  MSTSU1(IZ) NUMERIC
           MOVE  MSTSU1(IZ)         TO   S-STSU1
       ELSE
           MOVE  ZERO               TO   S-STSU1
       END-IF
       IF  MSTSU2(IZ) NUMERIC
           MOVE  MSTSU2(IZ)         TO   S-STSU2
       ELSE
           MOVE  ZERO               TO   S-STSU2
       END-IF
       IF  MSTSU3(IZ) NUMERIC
           MOVE  MSTSU3(IZ)         TO   S-STSU3
       ELSE
           MOVE  ZERO               TO   S-STSU3
       END-IF
       IF  MSTSU4(IZ) NUMERIC
           MOVE  MSTSU4(IZ)         TO   S-STSU4
       ELSE
           MOVE  ZERO               TO   S-STSU4
       END-IF
       IF  MSTSU5(IZ) NUMERIC
           MOVE  MSTSU5(IZ)         TO   S-STSU5
       ELSE
           MOVE  ZERO               TO   S-STSU5
       END-IF
       COMPUTE M-GK-STSU = S-STSU1 + S-STSU2 + S-STSU3 +
                           S-STSU4 + S-STSU5
*****  DISPLAY "M-GK-STSU = " M-GK-STSU  UPON  CONS
*****  DISPLAY "TEISUU(IZ)= " TEISUU(IZ) UPON  CONS
       IF  TEISUU(IZ) NOT =  M-GK-STSU
           IF  ERR-FLG  =  ZERO
               MOVE  18             TO   ERR-FLG
           END-IF
       END-IF
*エラー判定
       IF  ERR-FLG  NOT =  ZERO
           MOVE     "C"    TO   EDIT-CURSOR OF MSTNO1(IZ)
           MOVE     "R"    TO   EDIT-OPTION OF MSTNO1(IZ)
           MOVE     "R"    TO   EDIT-OPTION OF MSTSU1(IZ)
           MOVE     "R"    TO   EDIT-OPTION OF MSTNO2(IZ)
           MOVE     "R"    TO   EDIT-OPTION OF MSTSU2(IZ)
           MOVE     "R"    TO   EDIT-OPTION OF MSTNO3(IZ)
           MOVE     "R"    TO   EDIT-OPTION OF MSTSU3(IZ)
           MOVE     "R"    TO   EDIT-OPTION OF MSTNO4(IZ)
           MOVE     "R"    TO   EDIT-OPTION OF MSTSU4(IZ)
           MOVE     "R"    TO   EDIT-OPTION OF MSTNO5(IZ)
           MOVE     "R"    TO   EDIT-OPTION OF MSTSU5(IZ)
       END-IF
      END-IF
     END-PERFORM.
*
 220-GRP03-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    明細情報　発注数　訂正数チェック                          *
*--------------------------------------------------------------*
 MEISAI-CHK-SEC         SECTION.
     MOVE     "MEISAI-CHK-SEC"         TO   S-NAME.
     MOVE      ZERO                    TO   ERR-FLG.
*
     PERFORM  VARYING   IZ   FROM  1  BY  1  UNTIL  IZ  >  5
       MOVE   ZERO                  TO   CHK-CNT
      IF  TENPO(IZ)  NOT NUMERIC
      AND TENPO(IZ)  NUMERIC
*ストックＮＯ１
       IF  MSTNO1(IZ)  NOT =  SPACE   *>　ＳＴ１　入力
           IF  MSTSU1(IZ)  =  ZERO    *>  数量１　０
                IF   ERR-FLG   =    ZERO
*****  DISPLAY "M1-12" UPON CONS
                     MOVE      12   TO   ERR-FLG
                END-IF
           ELSE                   *>  数量１　入力　正常
                ADD       1    TO   CHK-CNT
           END-IF
       ELSE                       *>　ＳＴ１　空白
           IF  MSTSU1(IZ) NOT = ZERO  *>  数量１　０以外
           AND MSTSU1(IZ) NUMERIC
                IF   ERR-FLG   =    ZERO
*****  DISPLAY "M1-14" UPON CONS
                     MOVE      14   TO   ERR-FLG
                END-IF
            ELSE                  *>  数量１　０
                IF   ERR-FLG   =    ZERO
*****  DISPLAY "M1-15" " = " IZ  UPON CONS
*****  DISPLAY "TENPO = " TENPO(IZ) UPON CONS
                     MOVE      15   TO   ERR-FLG
                END-IF
           END-IF
       END-IF
*ストックＮＯ２
       IF  MSTNO2(IZ)  NOT =  SPACE   *>　ＳＴ２　入力
           IF  MSTSU2(IZ)  =  ZERO    *>  数量２　０
                IF   ERR-FLG   =    ZERO
*****  DISPLAY "M2-12" UPON CONS
                     MOVE      12   TO   ERR-FLG
                END-IF
           ELSE                   *>  数量２　入力　正常
                ADD       1    TO   CHK-CNT
                IF  CHK-CNT NOT = 2
                    IF   ERR-FLG   =    ZERO
*****  DISPLAY "M2-16" UPON CONS
                         MOVE      16   TO   ERR-FLG
                    END-IF
                END-IF
           END-IF
       ELSE                       *>　ＳＴ２　空白
           IF  MSTSU2(IZ) NOT = ZERO  *>  数量１２０以外
           AND MSTSU2(IZ) NUMERIC
                IF   ERR-FLG   =    ZERO
*****  DISPLAY "M2-14" UPON CONS
                     MOVE      14   TO   ERR-FLG
                END-IF
           END-IF
       END-IF
*ストックＮＯ３
       IF  MSTNO3(IZ)  NOT =  SPACE   *>　ＳＴ３　入力
           IF  MSTSU3(IZ)  =  ZERO    *>  数量３　０
                IF   ERR-FLG   =    ZERO
*****  DISPLAY "M3-12" UPON CONS
                     MOVE      12   TO   ERR-FLG
                END-IF
           ELSE                   *>  数量３　入力　正常
                ADD       1    TO   CHK-CNT
                IF  CHK-CNT NOT = 3
                    IF   ERR-FLG   =    ZERO
*****  DISPLAY "M3-16" UPON CONS
                         MOVE      16   TO   ERR-FLG
                    END-IF
                END-IF
           END-IF
       ELSE                       *>　ＳＴ３　空白
           IF  MSTSU3(IZ) NOT = ZERO  *>  数量３　０以外
           AND MSTSU3(IZ) NUMERIC
                IF   ERR-FLG   =    ZERO
*****  DISPLAY "M3-14" UPON CONS
                     MOVE      14   TO   ERR-FLG
                END-IF
           END-IF
       END-IF
*ストックＮＯ４
       IF  MSTNO4(IZ)  NOT =  SPACE   *>　ＳＴ４　入力
           IF  MSTSU4(IZ)  =  ZERO    *>  数量４　０
                IF   ERR-FLG   =    ZERO
*****  DISPLAY "M4-12" UPON CONS
                     MOVE      12   TO   ERR-FLG
                END-IF
           ELSE                   *>  数量４　入力　正常
                ADD       1    TO   CHK-CNT
                IF  CHK-CNT NOT = 4
                    IF   ERR-FLG   =    ZERO
*****  DISPLAY "M4-16" UPON CONS
                         MOVE      16   TO   ERR-FLG
                    END-IF
                END-IF
           END-IF
       ELSE                       *>　ＳＴ２　空白
           IF  MSTSU4(IZ) NOT = ZERO  *>  数量１２０以外
           AND MSTSU4(IZ) NUMERIC
                IF   ERR-FLG   =    ZERO
*****  DISPLAY "M4-14" UPON CONS
                     MOVE      14   TO   ERR-FLG
                END-IF
           END-IF
       END-IF
*ストックＮＯ５
       IF  MSTNO5(IZ)  NOT =  SPACE   *>　ＳＴ５　入力
           IF  MSTSU5(IZ)  =  ZERO    *>  数量５　０
                IF   ERR-FLG   =    ZERO
*****  DISPLAY "M5-12" UPON CONS
                     MOVE      12   TO   ERR-FLG
                END-IF
           ELSE                   *>  数量５　入力　正常
                ADD       1    TO   CHK-CNT
                IF  CHK-CNT NOT = 5
                    IF   ERR-FLG   =    ZERO
*****  DISPLAY "M5-16" UPON CONS
                         MOVE      16   TO   ERR-FLG
                    END-IF
                END-IF
           END-IF
       ELSE                       *>　ＳＴ５　空白
           IF  MSTSU5(IZ) NOT = ZERO  *>  数量５　０以外
           AND MSTSU5(IZ) NUMERIC
                IF   ERR-FLG   =    ZERO
*****  DISPLAY "M5-14" UPON CONS
                     MOVE      14   TO   ERR-FLG
                END-IF
           END-IF
       END-IF
*出荷数＝数量内訳合計チェック
       IF  MSTSU1(IZ) NUMERIC
           MOVE  MSTSU1(IZ)         TO   S-STSU1
       ELSE
           MOVE  ZERO               TO   S-STSU1
       END-IF
       IF  MSTSU2(IZ) NUMERIC
           MOVE  MSTSU2(IZ)         TO   S-STSU2
       ELSE
           MOVE  ZERO               TO   S-STSU2
       END-IF
       IF  MSTSU3(IZ) NUMERIC
           MOVE  MSTSU3(IZ)         TO   S-STSU3
       ELSE
           MOVE  ZERO               TO   S-STSU3
       END-IF
       IF  MSTSU4(IZ) NUMERIC
           MOVE  MSTSU4(IZ)         TO   S-STSU4
       ELSE
           MOVE  ZERO               TO   S-STSU4
       END-IF
       IF  MSTSU5(IZ) NUMERIC
           MOVE  MSTSU5(IZ)         TO   S-STSU5
       ELSE
           MOVE  ZERO               TO   S-STSU5
       END-IF
       COMPUTE M-GK-STSU = S-STSU1 + S-STSU2 + S-STSU3 +
                           S-STSU4 + S-STSU5
       IF  TEISUU(IZ) NOT =  M-GK-STSU
           IF  ERR-FLG  =  ZERO
               MOVE  18             TO   ERR-FLG
           END-IF
       END-IF
*エラー判定
       IF  ERR-FLG  NOT =  ZERO
           MOVE     "C"    TO   EDIT-CURSOR OF MSTNO1(IZ)
           MOVE     "R"    TO   EDIT-OPTION OF MSTNO1(IZ)
           MOVE     "R"    TO   EDIT-OPTION OF MSTSU1(IZ)
           MOVE     "R"    TO   EDIT-OPTION OF MSTNO2(IZ)
           MOVE     "R"    TO   EDIT-OPTION OF MSTSU2(IZ)
           MOVE     "R"    TO   EDIT-OPTION OF MSTNO3(IZ)
           MOVE     "R"    TO   EDIT-OPTION OF MSTSU3(IZ)
           MOVE     "R"    TO   EDIT-OPTION OF MSTNO4(IZ)
           MOVE     "R"    TO   EDIT-OPTION OF MSTSU4(IZ)
           MOVE     "R"    TO   EDIT-OPTION OF MSTNO5(IZ)
           MOVE     "R"    TO   EDIT-OPTION OF MSTSU5(IZ)
       END-IF
      END-IF
     END-PERFORM.
*
 MEISAI-CHK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｶﾞﾒﾝ ﾀｲﾋ ｼｮﾘ                                *
*--------------------------------------------------------------*
 GAMEN-TAIHI-SEC        SECTION.
     MOVE     "GAMEN-TAIHI-SEC"   TO   S-NAME.
*
     PERFORM 900-DSP-WRITE2.
     COMPUTE  IY   =    CNT-PAGE  *    5   -    5.
     PERFORM  VARYING   IZ   FROM  1  BY  1  UNTIL  IZ  >  5
         COMPUTE   IX   =    IY   +    IZ
              IF   TENPO(IZ)      IS NUMERIC
                   MOVE      MSTNO1(IZ)     TO   WRK-STNO1(IX)
                   MOVE      MSTSU1(IZ)     TO   WRK-STSU1(IX)
                   MOVE      MSTNO2(IZ)     TO   WRK-STNO2(IX)
                   MOVE      MSTSU2(IZ)     TO   WRK-STSU2(IX)
                   MOVE      MSTNO3(IZ)     TO   WRK-STNO3(IX)
                   MOVE      MSTSU3(IZ)     TO   WRK-STSU3(IX)
                   MOVE      MSTNO4(IZ)     TO   WRK-STNO4(IX)
                   MOVE      MSTSU4(IZ)     TO   WRK-STSU4(IX)
                   MOVE      MSTNO5(IZ)     TO   WRK-STNO5(IX)
                   MOVE      MSTSU5(IZ)     TO   WRK-STSU5(IX)
              ELSE
                   MOVE      "X"  TO   EDIT-STATUS OF MSTNO1(IZ)
                   MOVE      "X"  TO   EDIT-STATUS OF MSTSU1(IZ)
                   MOVE      "X"  TO   EDIT-STATUS OF MSTNO2(IZ)
                   MOVE      "X"  TO   EDIT-STATUS OF MSTSU2(IZ)
                   MOVE      "X"  TO   EDIT-STATUS OF MSTNO3(IZ)
                   MOVE      "X"  TO   EDIT-STATUS OF MSTSU3(IZ)
                   MOVE      "X"  TO   EDIT-STATUS OF MSTNO4(IZ)
                   MOVE      "X"  TO   EDIT-STATUS OF MSTSU4(IZ)
                   MOVE      "X"  TO   EDIT-STATUS OF MSTNO5(IZ)
                   MOVE      "X"  TO   EDIT-STATUS OF MSTSU5(IZ)
              END-IF
     END-PERFORM.
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
     PERFORM   GAMEN-TAIHI-SEC.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
         WHEN PF04
              MOVE      0         TO   GR-NO
              MOVE      SPACE     TO   CHK-FLG
         WHEN PF06
              MOVE      3         TO   GR-NO
              MOVE      1         TO   SAKUBA-CHG-FLG
         WHEN PF11
                   PERFORM 900-DSP-WRITE2
                   IF   CNT-PAGE  <=   1
                        MOVE      10   TO   ERR-FLG
                   ELSE
                        ADD       -1   TO   CNT-PAGE
                        PERFORM   GAMEN-SET-SEC
                   END-IF
         WHEN PF12
                   PERFORM 900-DSP-WRITE2
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
                        MOVE      13   TO   ERR-FLG
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
*    LEVEL  3      更新制御　　　                              *
*--------------------------------------------------------------*
 240-UPDATE-SEC           SECTION.
     MOVE     "240-UPDATE-SEC"    TO   S-NAME.
*
*ストックＮＯ管理ファイル更新
     PERFORM  VARYING   IX   FROM      1    BY   1
                             UNTIL     IX   >    CNT-MEISAI
         IF   WRK-TENPO(IX)  NOT =     ZERO
              PERFORM   KOUSIN1-SEC
         END-IF
     END-PERFORM.
*取消を押さない限り、バッチ番号は固定表示
     MOVE    "CHK"                TO   CHK-FLG.
     MOVE     JDATE               TO   WK-B-HIDUKE.
     MOVE     JTIME               TO   WK-B-TIME.
     MOVE     TORICD              TO   WK-B-TORICD.
     MOVE     SOKO                TO   WK-B-SOKCD.
     MOVE     NOUHIN              TO   WK-B-NOUDT.
*画面初期化ＳＥＣへ
     MOVE     0                   TO   GR-NO.
*
 240-UPDATE-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL         ｺｳｼﾝ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 KOUSIN1-SEC             SECTION.
     MOVE    "KOUSIN1-SEC"   TO   S-NAME.
*
*****MOVE     SPACE          TO   STN-REC.
*****INITIALIZE                   STN-REC.
     MOVE     JDATE          TO   STN-F01.
     MOVE     JTIME          TO   STN-F02.
     MOVE     TORICD         TO   STN-F03.
     MOVE     SOKO           TO   STN-F04.
     MOVE     JANCD          TO   STN-F19.
     MOVE     WRK-NOUDT(IX)  TO   STN-F05.
     MOVE     WRK-TENPO(IX)  TO   STN-F06.
     MOVE     WRK-DENNO(IX)  TO   STN-F07.
     MOVE     WRK-GYONO(IX)  TO   STN-F08.
*
     READ     STNOKRF
         INVALID
              DISPLAY NC"＃ファイル更新異常！！＃＃" UPON CONS
              DISPLAY NC"＃　ＫＥＹ無し！！　　＃＃" UPON CONS
              DISPLAY NC"＃" " KEY1 = " DE2-F46      UPON CONS
              DISPLAY NC"＃" " KEY2 = " DE2-F47      UPON CONS
              DISPLAY NC"＃" " KEY3 = " DE2-F01      UPON CONS
              DISPLAY NC"＃" " KEY4 = " DE2-F48      UPON CONS
              DISPLAY NC"＃" " KEY5 = " DE2-F02      UPON CONS
              DISPLAY NC"＃" " KEY6 = " DE2-F04      UPON CONS
              DISPLAY NC"＃" " KEY7 = " DE2-F051     UPON CONS
              DISPLAY NC"＃" " KEY8 = " DE2-F07      UPON CONS
              DISPLAY NC"＃" " KEY9 = " DE2-F112     UPON CONS
              DISPLAY NC"＃" " KEY10= " DE2-F03      UPON CONS
              MOVE    4000     TO       PROGRAM-STATUS
              STOP  RUN
     END-READ.
*
*****DISPLAY "STN-REC = " STN-REC UPON CONS.
     MOVE    WRK-SYUKASU(IX)      TO   STN-F14.
     COMPUTE STN-F17 = STN-F14 *  STN-F15.
     COMPUTE STN-F18 = STN-F14 *  STN-F16.
     MOVE    WRK-STNO1  (IX)      TO   STN-F30.
     MOVE    WRK-STSU1  (IX)      TO   STN-F31.
     MOVE    WRK-STNO2  (IX)      TO   STN-F32.
     MOVE    WRK-STSU2  (IX)      TO   STN-F33.
     MOVE    WRK-STNO3  (IX)      TO   STN-F34.
     MOVE    WRK-STSU3  (IX)      TO   STN-F35.
     MOVE    WRK-STNO4  (IX)      TO   STN-F36.
     MOVE    WRK-STSU4  (IX)      TO   STN-F37.
     MOVE    WRK-STNO5  (IX)      TO   STN-F38.
     MOVE    WRK-STSU5  (IX)      TO   STN-F39.
     MOVE    SYS-DATEW            TO   STN-F98.
     MOVE    SYS-TIMEW            TO   STN-F99.
     REWRITE STN-REC.
*
 KOUSIN1-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  READ                              *
*--------------------------------------------------------------*
 900-DSP-WRITE2         SECTION.
*
     MOVE     "SCREEN"       TO   DSP-GRP.
     MOVE     WK-SYSYMD           TO   SYSYMD.
     ACCEPT   SYS-TIME2      FROM TIME.
     MOVE     SYS-TIMEW           TO   SYSTIM.
*
     PERFORM  900-DSP-WRITE.
*
 900-DSP-WRITE2-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  READ                              *
*--------------------------------------------------------------*
 900-DSP-READ           SECTION.
     MOVE     "900-DSP-READ"      TO   S-NAME.
*
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
         IF   GR-NO     =    2
              MOVE      GUIDE02   TO   GUIDE
         ELSE
           IF   CNT-MEISAI     <=   10
                MOVE      GUIDE02   TO   GUIDE
           ELSE
                IF   CNT-PAGE  <=   1
                     MOVE      GUIDE03   TO   GUIDE
                ELSE
                     IF   CNT-PAGE  =    CNT-MAXPAGE
                          MOVE      GUIDE04   TO   GUIDE
                     ELSE
                          MOVE      GUIDE05   TO   GUIDE
                     END-IF
                END-IF
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
     WRITE    FST00601.
 900-DSP-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＨＥＡＤ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-HEAD-RTN           SECTION.
     MOVE     " "            TO   EDIT-CURSOR OF JDATE
                                  EDIT-CURSOR OF JTIME
                                  EDIT-CURSOR OF TORICD
                                  EDIT-CURSOR OF SOKO
                                  EDIT-CURSOR OF JANCD
                                  EDIT-CURSOR OF NOUHIN
                                  EDIT-CURSOR OF HTENCD.
*
     MOVE     "M"            TO   EDIT-OPTION OF JDATE
                                  EDIT-OPTION OF JTIME
                                  EDIT-OPTION OF TORICD
                                  EDIT-OPTION OF SOKO
                                  EDIT-OPTION OF JANCD
                                  EDIT-OPTION OF NOUHIN
                                  EDIT-OPTION OF HTENCD.
*
 CLR-HEAD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      一括変更部・明細部クリア　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-BODY1-RTN          SECTION.
*
     MOVE     " "            TO   EDIT-CURSOR OF DSTNO.
     MOVE     "M"            TO   EDIT-OPTION OF DSTNO.
*
     PERFORM  VARYING   IX   FROM  1   BY   1    UNTIL  IX > 5
         MOVE     " "        TO   EDIT-CURSOR OF MSTNO1(IX)
         MOVE     "M"        TO   EDIT-OPTION OF MSTNO1(IX)
         MOVE     " "        TO   EDIT-STATUS OF MSTNO1(IX)
         MOVE     " "        TO   EDIT-CURSOR OF MSTSU1(IX)
         MOVE     "M"        TO   EDIT-OPTION OF MSTSU1(IX)
         MOVE     " "        TO   EDIT-STATUS OF MSTSU1(IX)
         MOVE     " "        TO   EDIT-CURSOR OF MSTNO2(IX)
         MOVE     "M"        TO   EDIT-OPTION OF MSTNO2(IX)
         MOVE     " "        TO   EDIT-STATUS OF MSTNO2(IX)
         MOVE     " "        TO   EDIT-CURSOR OF MSTSU2(IX)
         MOVE     "M"        TO   EDIT-OPTION OF MSTSU2(IX)
         MOVE     " "        TO   EDIT-STATUS OF MSTSU2(IX)
         MOVE     " "        TO   EDIT-CURSOR OF MSTNO3(IX)
         MOVE     "M"        TO   EDIT-OPTION OF MSTNO3(IX)
         MOVE     " "        TO   EDIT-STATUS OF MSTNO3(IX)
         MOVE     " "        TO   EDIT-CURSOR OF MSTSU3(IX)
         MOVE     "M"        TO   EDIT-OPTION OF MSTSU3(IX)
         MOVE     " "        TO   EDIT-STATUS OF MSTSU3(IX)
         MOVE     " "        TO   EDIT-CURSOR OF MSTNO4(IX)
         MOVE     "M"        TO   EDIT-OPTION OF MSTNO4(IX)
         MOVE     " "        TO   EDIT-STATUS OF MSTNO4(IX)
         MOVE     " "        TO   EDIT-CURSOR OF MSTSU4(IX)
         MOVE     "M"        TO   EDIT-OPTION OF MSTSU4(IX)
         MOVE     " "        TO   EDIT-STATUS OF MSTSU4(IX)
         MOVE     " "        TO   EDIT-CURSOR OF MSTNO5(IX)
         MOVE     "M"        TO   EDIT-OPTION OF MSTNO5(IX)
         MOVE     " "        TO   EDIT-STATUS OF MSTNO5(IX)
         MOVE     " "        TO   EDIT-CURSOR OF MSTSU5(IX)
         MOVE     "M"        TO   EDIT-OPTION OF MSTSU5(IX)
         MOVE     " "        TO   EDIT-STATUS OF MSTSU5(IX)
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
*--------------------------------------------------------------*
*    作場マスタ　読込
*--------------------------------------------------------------*
 ZSOKMS-READ-SEC       SECTION.
*
     READ  ZSOKMS
           INVALID
           MOVE  "INV"  TO   ZSOKMS-INV-FLG
           NOT  INVALID
           MOVE  SPACE  TO   ZSOKMS-INV-FLG
     END-READ.
*
 ZSOKMS-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    売上伝票ファイル読込　　　　　　
*--------------------------------------------------------------*
 SHTDENF-READ-SEC         SECTION.
*
     MOVE      "SHTDENF-READ-SEC"  TO   S-NAME.
*
     MOVE  SPACE                   TO   DE2-REC.
     INITIALIZE                         DE2-REC.
     MOVE  STN-F01                 TO   DE2-F46.
     MOVE  STN-F02                 TO   DE2-F47.
     MOVE  STN-F03                 TO   DE2-F01.
     MOVE  STN-F04                 TO   DE2-F48.
     MOVE  STN-F07                 TO   DE2-F02.
     MOVE  0                       TO   DE2-F04.
     MOVE  40                      TO   DE2-F051.
     MOVE  STN-F06                 TO   DE2-F07.
     MOVE  STN-F05                 TO   DE2-F112.
     MOVE  STN-F08                 TO   DE2-F03.
     READ  SHTDENF
        INVALID
                MOVE  "INV"        TO   SHTDENF-INV-FLG
        NOT INVALID
                MOVE  SPACE        TO   SHTDENF-INV-FLG
     END-READ.
*
 SHTDENF-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
