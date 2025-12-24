# SSY8871L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY8871L.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　ホーマックセンター納品対応　　　　*
*    モジュール名　　　　：　納品ラベル発行　　　　　　　　　　*
*    作成日／更新日　　　：　2014/08/19                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　ダイキセンター納品データを読み、　*
*                        ：　納品ラベルを出力する。　　　　　　*
*    作成日／更新日　　　：　2018/01/26                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　形状区分＝４（他）→大かごへ変更　*
*    作成日／更新日　　　：　2019/02/21                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　発注種別区分対応　　　　　　　　　*
*    作成日／更新日　　　：　2019/03/18                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　発注種別変換マスタ対応　　　　　　*
*                        ：　                                  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY8871L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          14/08/19.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       GP6000.
 OBJECT-COMPUTER.       GP6000.
 SPECIAL-NAMES.
     YA            IS        CHR-2
     YB-21         IS        CHR-21
     YB            IS        CHR-15
     CONSOLE       IS        CONS
     STATION       IS        STAT.
****************************************************************
 INPUT-OUTPUT              SECTION.
****************************************************************
 FILE-CONTROL.
*----<<センター納品データ　　　　　　>>----*
     SELECT   DSCENTF   ASSIGN    TO        DA-01-VI-DSCENTL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DKC-F01   DKC-F02
                                            DKC-F03   DKC-F04
                                            DKC-F05   DKC-F06
                                            DKC-F16   DKC-F07
                        FILE      STATUS    DKC-ST.
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TOK-F01
                        FILE      STATUS    TOK-ST.
*----<< 店舗マスタ >>--*
     SELECT   TENMS1    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TEN-F52  TEN-F011
                        FILE      STATUS    TEN-ST.
*----<<プリント>>----*
     SELECT   PRTFILE   ASSIGN    TO        LP-04-PRTF
                        FILE      STATUS    PRT-ST.
*----<<ラベルデータ　　　　　　>>----*
     SELECT   DSLBLF    ASSIGN    TO        DA-01-VI-DSLBLL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       LBL-F01   LBL-F02
                                            LBL-F03   LBL-F04
                                            LBL-F05   LBL-F06
                                            LBL-F11   LBL-F07
                                            LBL-F08   LBL-F09
                        FILE      STATUS    LBL-ST.
*#2019/03/18 NAV ST
*発注種別変換マスタ
     SELECT   DCMHSBL2  ASSIGN    TO        DA-01-VI-DCMHSBL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       HSB-F01  HSB-F03
                        FILE  STATUS   IS   HSB-ST.
*#2019/03/18 NAV ED
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<<センター納品データ　　　　　　>>----*
 FD  DSCENTF            LABEL RECORD   IS   STANDARD.
     COPY     DSCENTF   OF        XFDLIB
              JOINING   DKC       PREFIX.
*----<< 店舗マスタ >>--*
 FD  TENMS1             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<<プリントファイル　　　　　　　>>----*
 FD  PRTFILE
                        LABEL  RECORD  IS   OMITTED
                        LINAGE IS      60   LINES
                        DATA   RECORD  IS   PRT-REC.
 01  PRT-REC.
     03  FILLER            PIC X(200).
*----<<ラベルデータ　　　　　　>>----*
 FD  DSLBLF             LABEL RECORD   IS   STANDARD.
     COPY     DSLBLF    OF        XFDLIB
              JOINING   LBL       PREFIX.
*#2019/03/11 NAV ST
******************************************************************
*    発注種別変換マスタ
******************************************************************
 FD  DCMHSBL2           LABEL RECORD   IS   STANDARD.
     COPY     DCMHSBF   OF        XFDLIB
              JOINING   HSB       PREFIX.
*
*#2019/03/11 NAV ED
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  X(03)   VALUE SPACE.
     03  DKC-FLG        PIC  X(03)   VALUE SPACE.
     03  CHK-FLG        PIC  X(03)   VALUE SPACE.
     03  ZERO-FLG       PIC  9(01)   VALUE ZERO.
     03  PAGE-FLG       PIC  9(01)   VALUE ZERO.
     03  TENPO-FLG      PIC  9(01)   VALUE ZERO.
     03  NOUHIN-FLG     PIC  9(01)   VALUE ZERO.
*#2019/03/18 NAV ST
     03  DCMHSBL2-INV-FLG PIC  X(03)  VALUE  SPACE.
*#2019/03/18 NAV ED
 01  WK-CNT.
     03  IX             PIC  9(03).
     03  IY             PIC  9(03).
     03  IZ             PIC  9(03).
     03  IXA            PIC  9(03).
     03  DKC-CNT        PIC  9(07).
     03  LBL-CNT        PIC  9(07).
     03  TOK-CNT        PIC  9(07).
     03  TEN-CNT        PIC  9(07).
     03  WRITE-CNT      PIC  9(07).
     03  PAGE-CNT       PIC  9(03).
     03  LINE-CNT       PIC  9(02).
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
     03  DKC-ST         PIC  X(02).
     03  TOK-ST         PIC  X(02).
     03  TEN-ST         PIC  X(02).
     03  PRT-ST         PIC  X(02).
     03  LBL-ST         PIC  X(02).
*#2019/03/18 NAV ST
     03  HSB-ST         PIC  X(02).
*#2019/03/18 NAV ED
*
 01  MAX-LINE          PIC  9(02)      VALUE  60.
 01  PG-ID             PIC  X(08)      VALUE  "SSY8871L".
 01  WK-MSG1           PIC  N(14)
                       VALUE NC"指定のデータはありません。".
*----<< ﾜｰｸ >>--*
 01  WK-AREA.
   03  WK-TENCD        PIC  9(05).
   03  WK-NOUHIN       PIC  9(08).
*    漢字変換
 01  WK-KANJI.
     03  KANJI                    PIC  N(10)  VALUE
         NC"１２３４５６７８９０".
 01  WK-KANJIR                    REDEFINES   WK-KANJI.
     03  WK-SU                    OCCURS      10.
         05  SU                   PIC  N(01).
 01  WK-HENKAN-KANJI.
     03  WK-HENKAN-N.
         05  WK-HENKAN-1          PIC  N(01)  VALUE  SPACE.
         05  WK-HENKAN-2          PIC  N(01)  VALUE  SPACE.
         05  WK-HENKAN-3          PIC  N(01)  VALUE  SPACE.
 01  WK-HENKAN                    PIC  9(03).
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-YYMD           PIC  9(08).
 01  FILLER             REDEFINES      SYS-YYMD.
     03  SYS-YYYY       PIC  9(04).
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
****************************************************************
*    プリントエリア                                            *
****************************************************************
*--------------------------------------------------------------*
*    ヘッダ                                                    *
*--------------------------------------------------------------*
*
 01  HD1                         CHARACTER  TYPE  IS  CHR-21.
     03  HD1-A                   OCCURS     4.
*#2019/02/21 NAV ST
*******05  FILLER                PIC  X(15)  VALUE  SPACE.
*******05  HD1-01                PIC  N(05).
*######05  FILLER                PIC  X(09)  VALUE  SPACE.
*######05  HD1-01                PIC  N(07).
       05  FILLER                PIC  X(06)  VALUE  SPACE.
       05  HD1-01                PIC  N(08).
*#2019/02/21 NAV ED
       05  FILLER                PIC  X(05)  VALUE  SPACE.
*
 01  HD2                         CHARACTER  TYPE  IS  CHR-21.
     03  HD2-A                   OCCURS     4.
       05  FILLER                PIC  X(01)  VALUE  SPACE.
       05  HD2-N1                PIC  N(04).
*##                              NC"形状区分"
       05  FILLER                PIC  X(01)  VALUE  SPACE.
       05  HD2-01                PIC  N(03).
       05  FILLER                PIC  X(12)  VALUE  SPACE.
*
 01  HD3                         CHARACTER  TYPE  IS  CHR-21.
     03  HD3-A                   OCCURS     4.
       05  FILLER                PIC  X(01)  VALUE  SPACE.
       05  HD3-N1                PIC  N(04).
*##                              NC"該当部門"
       05  FILLER                PIC  X(01)  VALUE  SPACE.
       05  HD3-N2                PIC  N(01).
*##                                          VALUE  NC"＃"
       05  HD3-01                PIC  N(03).
       05  FILLER                PIC  X(09)  VALUE  SPACE.
*
 01  HD4                         CHARACTER  TYPE  IS  CHR-21.
     03  HD4-A                   OCCURS     4.
       05  FILLER                PIC  X(01)  VALUE  SPACE.
       05  HD4-N1                PIC  N(01).
*##                              NC"個".
*******05  FILLER                PIC  X(01)  VALUE  SPACE.
       05  HD4-N2                PIC  N(01).
*##                              NC"口"
       05  HD4-N3                PIC  N(01).
*##                              NC"数"
       05  HD4-01                PIC  N(03).
       05  HD4-N4                PIC  N(01).
*##                                          VALUE  NC"／"
       05  HD4-02                PIC  N(03).
       05  FILLER                PIC  X(04)  VALUE  SPACE.
*
 01  HD5.
     03  HD5-A                   OCCURS     4.
       05  FILLER                PIC  X(01)  VALUE  SPACE.
       05  HD5-N1                PIC  X(06).
*##                              "ﾍﾞﾝﾀﾞｰ"
       05  HD5-N2                PIC  N(01)
                                 CHARACTER  TYPE  IS  CHR-2.
*##                              NC"名"
       05  FILLER                PIC  X(04)  VALUE  SPACE.
       05  HD5-N3                PIC  N(09)
                                 CHARACTER  TYPE  IS  CHR-15.
*##                              NC"（株）サカタのタネ"
       05  FILLER                PIC  X(09)  VALUE  SPACE.
*
 01  HD6                         CHARACTER  TYPE  IS  CHR-2.
     03  HD6-A                   OCCURS     4.
       05  FILLER                PIC  X(01)  VALUE  SPACE.
       05  HD6-N1                PIC  N(03).
*##                              NC"連絡先"
       05  HD6-N2                PIC  X(03).
*##                              "FAX"
       05  HD6-N3                PIC  N(01).
*##                                          VALUE  NC"："
       05  FILLER                PIC  X(01)  VALUE  SPACE.
       05  HD6-N4                PIC  X(15).
*##                              "045-945-8817   "
       05  FILLER                PIC  X(07)  VALUE  SPACE.
*
 01  HD7                         CHARACTER  TYPE  IS  CHR-2.
     03  HD7-A                   OCCURS     4.
       05  FILLER                PIC  X(01)  VALUE  SPACE.
       05  HD7-N1                PIC  N(03).
*##                              NC"連絡先"
       05  HD7-N2                PIC  X(03).
*##                              "TEL"
       05  HD7-N3                PIC  N(01).
*##                                          VALUE  NC"："
       05  FILLER                PIC  X(01)  VALUE  SPACE.
       05  HD7-N4                PIC  X(15).
*##                              "045-945-8816   "
       05  FILLER                PIC  X(07)  VALUE  SPACE.
*
 01  HD8                         CHARACTER  TYPE  IS  CHR-2.
     03  HD8-A                   OCCURS     4.
       05  FILLER                PIC  X(01)  VALUE  SPACE.
       05  HD8-01                PIC  N(05).
       05  FILLER                PIC  X(01)  VALUE  SPACE.
       05  HD8-02                PIC  N(10)  VALUE  SPACE.
       05  FILLER                PIC  X(03)  VALUE  SPACE.
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
*
 LINKAGE                SECTION.
 01  PARA-JDATE             PIC   9(08).
 01  PARA-JTIME             PIC   9(04).
 01  PARA-TORICD            PIC   9(08).
 01  PARA-SOKO              PIC   X(02).
 01  PARA-BUMON             PIC   9(03).
 01  PARA-STDATE            PIC   9(08).
 01  PARA-EDDATE            PIC   9(08).
 01  PARA-STTEN             PIC   9(05).
 01  PARA-EDTEN             PIC   9(05).
 01  PARA-KEKA              PIC   X(01).
****************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE
                                       PARA-JTIME
                                       PARA-TORICD
                                       PARA-SOKO
                                       PARA-BUMON
                                       PARA-STDATE
                                       PARA-EDDATE
                                       PARA-STTEN
                                       PARA-EDTEN
                                       PARA-KEKA.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
 DKCERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DSCENTF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY8871L DKC ERROR " DKC-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     4000           TO        PROGRAM-STATUS.
     STOP     RUN.
 LBLERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DSLBLF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY8871L LBL ERROR " LBL-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     4000           TO        PROGRAM-STATUS.
     STOP     RUN.
*得意先マスタ
 TOKERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY8871L TOK ERROR " TOK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     4000           TO        PROGRAM-STATUS.
     STOP     RUN.
*店舗マスタ
 TENERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      TENMS1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY8871L TEN ERROR " TEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     4000           TO        PROGRAM-STATUS.
     STOP     RUN.
 PRTERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      PRTFILE.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY8871L PRTFILE ERROR " PRT-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     4000           TO        PROGRAM-STATUS.
     STOP     RUN.
 HSBERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DCMHSBL2.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY8871L DCMHSBL2 ERROR " HSB-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     4000           TO        PROGRAM-STATUS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN
              UNTIL     END-FLG   =  "END".
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSY8871L START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     DSCENTF   HTOKMS    TENMS1.
     OPEN     OUTPUT    PRTFILE.
     OPEN     OUTPUT    DSLBLF.
*#2019/03/18 NAV ST
     OPEN     INPUT     DCMHSBL2.
*#2019/03/18 NAV ED
     DISPLAY "PARA-JDATE   = " PARA-JDATE   UPON CONS.
     DISPLAY "PARA-JTIME   = " PARA-JTIME   UPON CONS.
     DISPLAY "PARA-TORICD  = " PARA-TORICD  UPON CONS.
     DISPLAY "PARA-SOKO    = " PARA-SOKO    UPON CONS.
     DISPLAY "PARA-BUMON   = " PARA-BUMON   UPON CONS.
     DISPLAY "PARA-STDATE  = " PARA-STDATE  UPON CONS.
     DISPLAY "PARA-EDDATE  = " PARA-EDDATE  UPON CONS.
     DISPLAY "PARA-STTEN   = " PARA-STTEN   UPON CONS.
     DISPLAY "PARA-EDTEN   = " PARA-EDTEN   UPON CONS.
*クリア
     INITIALIZE    WK-CNT  FLAGS.
     MOVE     SPACE          TO   HD1  HD2  HD3  HD4  HD5  HD6
                                  HD7  HD8.
*センター納品データスタート
     MOVE     SPACE          TO   DKC-REC.
     INITIALIZE                   DKC-REC.
     MOVE     PARA-JDATE     TO   DKC-F01.
     MOVE     PARA-JTIME     TO   DKC-F02.
     MOVE     PARA-TORICD    TO   DKC-F03.
     MOVE     PARA-SOKO      TO   DKC-F04.
     START    DSCENTF   KEY  >=   DKC-F01   DKC-F02
                                  DKC-F03   DKC-F04
                                  DKC-F05   DKC-F06
                                  DKC-F16   DKC-F07
         INVALID   KEY
              MOVE    "END"  TO   DKC-FLG
              GO   TO   100-INIT-RTN-EXIT
     END-START.
*    ラベルデータ作成
     PERFORM UNTIL      DKC-FLG   =   "END"
*            センター納品データ読込み
             PERFORM DSCENTF-READ-SEC
             IF      DKC-FLG    =    SPACE
*            ラベルデータ出力
                     PERFORM    DSLBLF-WT-SEC
             END-IF
     END-PERFORM.
     CLOSE   DSLBLF.
     OPEN    INPUT   DSLBLF.
*    ラベルデータ読込み
     PERFORM  DSLBLF-READ-SEC.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*
     IF       LINE-CNT  =        6
**************MOVE      SPACE    TO        PRT-REC
**************WRITE     PRT-REC  AFTER     PAGE
              MOVE      ZERO     TO        LINE-CNT
     END-IF.
*
     ADD      1         TO       IY.
*    タイトルセット
     MOVE NC"形状区分"                     TO  HD2-N1(IY).
     MOVE NC"該当部門"                     TO  HD3-N1(IY).
     MOVE NC"＃"                           TO  HD3-N2(IY).
     MOVE NC"個"                           TO  HD4-N1(IY).
     MOVE NC"口"                           TO  HD4-N2(IY).
     MOVE NC"数"                           TO  HD4-N3(IY).
     MOVE NC"／"                           TO  HD4-N4(IY).
     MOVE "ﾍﾞﾝﾀﾞｰ"                         TO  HD5-N1(IY).
     MOVE NC"名"                           TO  HD5-N2(IY).
     MOVE NC"（株）サカタのタネ"           TO  HD5-N3(IY).
     MOVE NC"連絡先"                       TO  HD6-N1(IY).
     MOVE "FAX"                            TO  HD6-N2(IY).
     MOVE NC"："                           TO  HD6-N3(IY).
     MOVE "045-945-8817  "                 TO  HD6-N4(IY).
     MOVE NC"連絡先"                       TO  HD7-N1(IY).
     MOVE "TEL"                            TO  HD7-N2(IY).
     MOVE NC"："                           TO  HD7-N3(IY).
     MOVE "045-945-8816  "                 TO  HD7-N4(IY).
*    形状区分
     EVALUATE LBL-F08
         WHEN 1
              MOVE      NC"小物"       TO   HD2-01(IY)
         WHEN 2
              MOVE      NC"異形"       TO   HD2-01(IY)
         WHEN 3
              MOVE      NC"ケース"     TO   HD2-01(IY)
*#2018/01/26 NAV ST
         WHEN 4
**************MOVE      NC"他"         TO   HD2-01(IY)
              MOVE      NC"大カゴ"     TO   HD2-01(IY)
*#2018/01/26 NAV ED
     END-EVALUATE.
*    定番／特売区分
     EVALUATE LBL-F11
         WHEN "1"
              MOVE      NC"　　　　定　期" TO   HD1-01(IY)
         WHEN "2"
              MOVE      NC"　　　　特　売" TO   HD1-01(IY)
         WHEN "3"
              MOVE      NC"　　　本部投入" TO   HD1-01(IY)
         WHEN "4"
              MOVE      NC"　　　新店改装" TO   HD1-01(IY)
         WHEN "6"
              MOVE      NC"緊急ダイレクト" TO   HD1-01(IY)
*#2019/02/21 NAV ST
         WHEN "F"
              MOVE      NC"客注ダイレクト" TO   HD1-01(IY)
         WHEN "A"
              MOVE      NC"　本部在庫補充" TO   HD1-01(IY)
         WHEN "B"
              MOVE      NC"専用_在庫補充" TO   HD1-01(IY)
         WHEN "C"
              MOVE      NC"　　　　用度品" TO   HD1-01(IY)
         WHEN "D"
              MOVE      NC"　　　　改　廃" TO   HD1-01(IY)
         WHEN "E"
              MOVE      NC"　　　ＢＹ改廃" TO   HD1-01(IY)
*#2019/02/21 NAV ED
         WHEN OTHER
              MOVE      NC"　　定　期　　" TO   HD1-01(IY)
     END-EVALUATE.
*#2019/03/18 NAV ST 発注種別変換マスタより取得
     MOVE     LBL-F03                      TO   HSB-F01.
     MOVE     LBL-F11                      TO   HSB-F03.
     PERFORM  DCMHSBL2-READ-SEC.
     IF  DCMHSBL2-INV-FLG  =  SPACE
              MOVE   HSB-F07               TO   HD1-01(IY)
     ELSE
              MOVE   NC"　　定　期　　"    TO   HD1-01(IY)
     END-IF.
*#2019/03/18 NAV ED 発注種別変換マスタより取得
*    部門
     MOVE     SPACE          TO        WK-HENKAN-N.
     MOVE     1              TO        ZERO-FLG.
     MOVE     LBL-F07        TO        WK-HENKAN.
     PERFORM  VARYING   IZ   FROM  1 BY 1  UNTIL  IZ > 3
              PERFORM   KANJI-HENKAN-SEC
     END-PERFORM.
     MOVE     WK-HENKAN-N    TO        HD3-01(IY).
*    個口数（分子）
     MOVE     SPACE          TO        WK-HENKAN-N.
     MOVE     ZERO           TO        ZERO-FLG.
     MOVE     LBL-F09        TO        WK-HENKAN.
     PERFORM  VARYING   IZ   FROM  1 BY 1  UNTIL  IZ > 3
              PERFORM   KANJI-HENKAN-SEC
     END-PERFORM.
     MOVE     WK-HENKAN-N    TO        HD4-01(IY).
*    個口数（分母）
     MOVE     SPACE          TO        WK-HENKAN-N.
     MOVE     ZERO           TO        ZERO-FLG.
     MOVE     LBL-F10        TO        WK-HENKAN.
     PERFORM  VARYING   IZ   FROM  1 BY 1  UNTIL  IZ > 3
              PERFORM   KANJI-HENKAN-SEC
     END-PERFORM.
     MOVE     WK-HENKAN-N    TO        HD4-02(IY).
*
     EVALUATE LBL-F11
         WHEN "1"   MOVE NC"定期"           TO   HD8-01(IY)
         WHEN "2"   MOVE NC"特売"           TO   HD8-01(IY)
         WHEN "3"   MOVE NC"投入"           TO   HD8-01(IY)
         WHEN "4"   MOVE NC"新店改装"       TO   HD8-01(IY)
         WHEN "6"   MOVE NC"緊急ダイレ"     TO   HD8-01(IY)
*#2019/02/21 NAV ST
         WHEN "F"   MOVE NC"客注ダイレ"     TO   HD8-01(IY)
         WHEN "A"   MOVE NC"本部在庫補"     TO   HD8-01(IY)
         WHEN "B"   MOVE NC"専用_在庫"     TO   HD8-01(IY)
         WHEN "C"   MOVE NC"用度品　　"     TO   HD8-01(IY)
         WHEN "D"   MOVE NC"改廃　　　"     TO   HD8-01(IY)
         WHEN "E"   MOVE NC"ＢＹ改廃　"     TO   HD8-01(IY)
*#2019/02/21 NAV ED
         WHEN OTHER MOVE ALL NC"＊"         TO   HD8-01(IY)
     END-EVALUATE.
*#2019/03/18 NAV ST 発注種別変換マスタより取得
     MOVE     LBL-F03                       TO   HSB-F01.
     MOVE     LBL-F11                       TO   HSB-F03.
     PERFORM  DCMHSBL2-READ-SEC.
     IF  DCMHSBL2-INV-FLG  =  SPACE
              MOVE   HSB-F08                TO   HD8-01(IY)
     ELSE
              MOVE   ALL NC"＊"             TO   HD8-01(IY)
     END-IF.
*#2019/03/18 NAV ED 発注種別変換マスタより取得
*店舗名を表示
     MOVE     LBL-F03                       TO   TEN-F52.
     MOVE     LBL-F06                       TO   TEN-F011.
     PERFORM  900-TEN-READ.
     MOVE     TEN-F03                       TO   HD8-02(IY).
*
     IF       IY   =    4
*        出力
         WRITE     PRT-REC      FROM   HD1       AFTER  2
         WRITE     PRT-REC      FROM   HD2       AFTER  1
         WRITE     PRT-REC      FROM   HD3       AFTER  1
         WRITE     PRT-REC      FROM   HD4       AFTER  1
         WRITE     PRT-REC      FROM   HD5       AFTER  1
         WRITE     PRT-REC      FROM   HD6       AFTER  1
         WRITE     PRT-REC      FROM   HD7       AFTER  1
         WRITE     PRT-REC      FROM   HD8       AFTER  1
         MOVE      SPACE        TO     PRT-REC
         WRITE     PRT-REC                       AFTER  1
         ADD       1            TO     LINE-CNT
         MOVE      ZERO         TO     IY
*
         PERFORM   VARYING  IX  FROM   1  BY  1  UNTIL  IX >= 4
              MOVE SPACE    TO    HD1-01(IX)
              MOVE SPACE    TO    HD2-01(IX)
              MOVE SPACE    TO    HD3-01(IX)
              MOVE SPACE    TO    HD4-01(IX)
              MOVE SPACE    TO    HD4-02(IX)
         END-PERFORM
         MOVE SPACE          TO   HD1  HD2  HD3  HD4  HD5  HD6
                                  HD7  HD8
     END-IF.
*
*    ラベルデータ読込み
     PERFORM DSLBLF-READ-SEC.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
*
     IF       IY   >=   1
*        出力
         WRITE     PRT-REC      FROM   HD1       AFTER  2
         WRITE     PRT-REC      FROM   HD2       AFTER  1
         WRITE     PRT-REC      FROM   HD3       AFTER  1
         WRITE     PRT-REC      FROM   HD4       AFTER  1
         WRITE     PRT-REC      FROM   HD5       AFTER  1
         WRITE     PRT-REC      FROM   HD6       AFTER  1
         WRITE     PRT-REC      FROM   HD7       AFTER  1
         WRITE     PRT-REC      FROM   HD8       AFTER  1
         MOVE      SPACE        TO     PRT-REC
         WRITE     PRT-REC                       AFTER  1
         ADD       1            TO     LINE-CNT
         MOVE      ZERO         TO     IY
     END-IF.
*
     IF  LINE-CNT  <=  ZERO
        DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃" UPON CONS
        DISPLAY NC"＃出力対象ラベルがありません＃" UPON CONS
        DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃" UPON CONS
         MOVE      "1"          TO     PARA-KEKA
     ELSE
         MOVE      "0"          TO     PARA-KEKA
     END-IF.
*
     CLOSE    DSCENTF HTOKMS TENMS1 PRTFILE DSLBLF.
*#2019/03/18 NAV ST
     CLOSE    DCMHSBL2.
*#2019/03/18 NAV ED
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSY8871L END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  センター納品データ読込み                    *
*--------------------------------------------------------------*
 DSCENTF-READ-SEC            SECTION.
     READ   DSCENTF     NEXT
         AT END
            MOVE  "END"  TO  DKC-FLG
            GO           TO  DSCENTF-READ-EXIT
         NOT   AT  END
            ADD    1     TO  DKC-CNT
     END-READ.
*    バッチ番号のチェック
     IF       PARA-JDATE  =  DKC-F01
     AND      PARA-JTIME  =  DKC-F02
     AND      PARA-TORICD =  DKC-F03
              CONTINUE
     ELSE
              MOVE     "END"     TO   DKC-FLG
              GO                 TO   DSCENTF-READ-EXIT
     END-IF.
     IF       PARA-SOKO   =  SPACE
              GO                 TO   DSCENTF-READ-0010
     END-IF.
*    抽出条件のチェック
     IF       PARA-SOKO      =   DKC-F04
              GO                 TO   DSCENTF-READ-0010
     ELSE
              GO                 TO   DSCENTF-READ-SEC
     END-IF.
 DSCENTF-READ-0010.
*    部門のチェック
***  DISPLAY "PARA-BUMON = " PARA-BUMON UPON CONS.
     IF       PARA-BUMON     =   999
*****OR       PARA-BUMON     =   SPACE
              CONTINUE
     ELSE
         IF   PARA-BUMON     =   DKC-F07
              CONTINUE
         ELSE
              GO                 TO   DSCENTF-READ-SEC
         END-IF
     END-IF.
*
 DSCENTF-READ-0020.
     IF  PARA-STDATE   <=   DKC-F05
     AND PARA-EDDATE   >=   DKC-F05
         CONTINUE
     ELSE
              GO                 TO   DSCENTF-READ-SEC
     END-IF.
*
 DSCENTF-READ-0030.
     IF  PARA-STTEN    <=   DKC-F06
     AND PARA-EDTEN    >=   DKC-F06
         CONTINUE
     ELSE
              GO                 TO   DSCENTF-READ-SEC
     END-IF.
*
 DSCENTF-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  ラベルデータ出力　　　　　                  *
*--------------------------------------------------------------*
 DSLBLF-WT-SEC              SECTION.
*    小物
     PERFORM  VARYING   IX   FROM      1    BY   1
              UNTIL     IX   >    DKC-F08
         MOVE      SPACE     TO   LBL-REC
         INITIALIZE               LBL-REC
         MOVE      DKC-F01   TO   LBL-F01
         MOVE      DKC-F02   TO   LBL-F02
         MOVE      DKC-F03   TO   LBL-F03
         MOVE      DKC-F04   TO   LBL-F04
         MOVE      DKC-F05   TO   LBL-F05
         MOVE      DKC-F06   TO   LBL-F06
         MOVE      DKC-F07   TO   LBL-F07
         MOVE      1         TO   LBL-F08
         MOVE      IX        TO   LBL-F09
         MOVE      DKC-F08   TO   LBL-F10
         MOVE      DKC-F16   TO   LBL-F11
         WRITE     LBL-REC
         ADD       1         TO   WRITE-CNT
     END-PERFORM.
*    異形
     PERFORM  VARYING   IX   FROM      1    BY   1
              UNTIL     IX   >    DKC-F09
         MOVE      SPACE     TO   LBL-REC
         INITIALIZE               LBL-REC
         MOVE      DKC-F01   TO   LBL-F01
         MOVE      DKC-F02   TO   LBL-F02
         MOVE      DKC-F03   TO   LBL-F03
         MOVE      DKC-F04   TO   LBL-F04
         MOVE      DKC-F05   TO   LBL-F05
         MOVE      DKC-F06   TO   LBL-F06
         MOVE      DKC-F07   TO   LBL-F07
         MOVE      2         TO   LBL-F08
         MOVE      IX        TO   LBL-F09
         MOVE      DKC-F09   TO   LBL-F10
         MOVE      DKC-F16   TO   LBL-F11
         WRITE     LBL-REC
         ADD       1         TO   WRITE-CNT
     END-PERFORM.
*    ケース
     PERFORM  VARYING   IX   FROM      1    BY   1
              UNTIL     IX   >    DKC-F10
         MOVE      SPACE     TO   LBL-REC
         INITIALIZE               LBL-REC
         MOVE      DKC-F01   TO   LBL-F01
         MOVE      DKC-F02   TO   LBL-F02
         MOVE      DKC-F03   TO   LBL-F03
         MOVE      DKC-F04   TO   LBL-F04
         MOVE      DKC-F05   TO   LBL-F05
         MOVE      DKC-F06   TO   LBL-F06
         MOVE      DKC-F07   TO   LBL-F07
         MOVE      3         TO   LBL-F08
         MOVE      IX        TO   LBL-F09
         MOVE      DKC-F10   TO   LBL-F10
         MOVE      DKC-F16   TO   LBL-F11
         WRITE     LBL-REC
         ADD       1         TO   WRITE-CNT
     END-PERFORM.
*    他
     PERFORM  VARYING   IX   FROM      1    BY   1
              UNTIL     IX   >    DKC-F11
         MOVE      SPACE     TO   LBL-REC
         INITIALIZE               LBL-REC
         MOVE      DKC-F01   TO   LBL-F01
         MOVE      DKC-F02   TO   LBL-F02
         MOVE      DKC-F03   TO   LBL-F03
         MOVE      DKC-F04   TO   LBL-F04
         MOVE      DKC-F05   TO   LBL-F05
         MOVE      DKC-F06   TO   LBL-F06
         MOVE      DKC-F07   TO   LBL-F07
         MOVE      4         TO   LBL-F08
         MOVE      IX        TO   LBL-F09
         MOVE      DKC-F11   TO   LBL-F10
         MOVE      DKC-F16   TO   LBL-F11
         WRITE     LBL-REC
         ADD       1         TO   WRITE-CNT
     END-PERFORM.
*
 DSLBLF-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  ラベルデータ読込み　　　                    *
*--------------------------------------------------------------*
 DSLBLF-READ-SEC            SECTION.
     READ   DSLBLF      NEXT
         AT END
            MOVE  "END"  TO  END-FLG
            GO           TO  DSLBLF-READ-EXIT
         NOT   AT  END
            ADD    1     TO  LBL-CNT
     END-READ.
*
 DSLBLF-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    取引先マスタ　 READ                          *
*--------------------------------------------------------------*
 900-TOK-READ           SECTION.
     READ     HTOKMS    INVALID
              MOVE      SPACE     TO   TOK-F02
     END-READ.
 900-TOK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    店舗マスタ　　 READ                          *
*--------------------------------------------------------------*
 900-TEN-READ           SECTION.
     READ     TENMS1    INVALID
              MOVE      SPACE     TO   TEN-F03
     END-READ.
 900-TEN-READ-EXIT.
     EXIT.
*============================================================*
*　　漢字変換処理　　　　　                構造■2.1.1       *
*============================================================*
 KANJI-HENKAN-SEC      SECTION.
*    数字を漢字に変換（ワーク変換テーブルにより）
     MOVE    WK-HENKAN(IZ:1)           TO     IXA.
     IF      IXA       =     ZERO
             MOVE      10              TO     IXA
     END-IF.
     EVALUATE     IZ
         WHEN     1
             IF   IXA        =    10
                  IF    ZERO-FLG  =    1
                        MOVE SU(IXA)   TO   WK-HENKAN-1
                  ELSE
                        MOVE SPACE     TO   WK-HENKAN-1
                  END-IF
             ELSE
                  MOVE       SU(IXA)   TO     WK-HENKAN-1
                  MOVE       1         TO     ZERO-FLG
             END-IF
         WHEN     2
             IF   IXA        =    10
                  IF    ZERO-FLG  =    1
                        MOVE SU(IXA)   TO   WK-HENKAN-2
                  ELSE
                        MOVE SPACE     TO   WK-HENKAN-2
                  END-IF
             ELSE
                  MOVE  SU(IXA)        TO   WK-HENKAN-2
             END-IF
         WHEN     3
             MOVE      SU(IXA)         TO     WK-HENKAN-3
     END-EVALUATE.
*
 KANJI-HENKAN-EXIT.
     EXIT.
****************************************************************
*　　発注種別変換マスタ索引
****************************************************************
 DCMHSBL2-READ-SEC         SECTION.
*
     READ     DCMHSBL2
         INVALID
           MOVE  "INV"     TO        DCMHSBL2-INV-FLG
         NOT INVALID
           MOVE  SPACE     TO        DCMHSBL2-INV-FLG
     END-READ.
*
 DCMHSBL2-READ-EXIT.
     EXIT.

```
