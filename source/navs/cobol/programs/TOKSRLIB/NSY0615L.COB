****************************************************************
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　ＤＣＭ仕入先統合　　　　　　　　　*
*    モジュール名　　　　：　納品ラベル発行（店直センター経由）*
*    作成日／更新日　　　：　2021/02/18                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　センター納品データを読み、納品ラ　*
*                        ：　ベルを出力する。　　　　　　　　　*
*                        ：　　　　　　　　　　　　　　        *
**履歴**********************************************************
*    2021/02/18  高橋　　新規作成（ＮＳＹ０６１５Ｌコピー）　　*
*    0000/00/00　　　　　　　　　　　　　　　　　　　　　　　　*
*    0000/00/00　　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            NSY0615L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          18/02/19.
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
     YB-22         IS        CHR-22
     CONSOLE       IS        CONS
     STATION       IS        STAT.
****************************************************************
 INPUT-OUTPUT              SECTION.
****************************************************************
 FILE-CONTROL.
*----<<センター納品データ　　　　　　>>----*
     SELECT   DDCENTF   ASSIGN    TO        DA-01-VI-DDCENTL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DKC-F01   DKC-F02
                                            DKC-F03   DKC-F04
                                            DKC-F05   DKC-F06
                                            DKC-F18   DKC-F19
                                            DKC-F07
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
     SELECT   LBLXXXF   ASSIGN    TO        DA-01-VI-DSLBLL1
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
 FD  DDCENTF            LABEL RECORD   IS   STANDARD.
     COPY     DDCENTF   OF        XFDLIB
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
 FD  LBLXXXF             LABEL RECORD   IS   STANDARD.
     COPY     LBLXXXF   OF        XFDLIB
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
     03  IX             PIC  9(04).
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
 01  PG-ID             PIC  X(08)      VALUE  "NSY0615L".
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
 01  HD0.
     03  HD0-A                   OCCURS     4.
       05  FILLER                PIC  X(01)  VALUE  SPACE.
       05  HD0-01                PIC  N(04)
                                 CHARACTER  TYPE  IS  CHR-2.
       05  HD0-02                PIC  X(01).
       05  HD0-03                PIC  N(14)
                                 CHARACTER  TYPE  IS  CHR-15.
       05  FILLER                PIC  X(03)  VALUE  SPACE.
*
 01  HD1                         CHARACTER  TYPE  IS  CHR-22.
     03  HD1-A                   OCCURS     4.
       05  FILLER                PIC  X(06)  VALUE  SPACE.
       05  HD1-01                PIC  N(08).
       05  FILLER                PIC  X(04)  VALUE  SPACE.
*
 01  HD2                         CHARACTER  TYPE  IS  CHR-21.
     03  HD2-A                   OCCURS     4.
       05  FILLER                PIC  X(01)  VALUE  SPACE.
       05  HD2-N1                PIC  N(04).
       05  FILLER                PIC  X(01)  VALUE  SPACE.
       05  HD2-01                PIC  N(03).
*******05  FILLER                PIC  X(12)  VALUE  SPACE.
       05  FILLER                PIC  X(11)  VALUE  SPACE.
*
 01  HD3                         CHARACTER  TYPE  IS  CHR-21.
     03  HD3-A                   OCCURS     4.
       05  FILLER                PIC  X(01)  VALUE  SPACE.
       05  HD3-N1                PIC  N(04).
       05  FILLER                PIC  X(01)  VALUE  SPACE.
       05  HD3-N2                PIC  N(01).
       05  HD3-01                PIC  N(03).
*******05  FILLER                PIC  X(09)  VALUE  SPACE.
       05  FILLER                PIC  X(08)  VALUE  SPACE.
*
 01  HD4                         CHARACTER  TYPE  IS  CHR-21.
     03  HD4-A                   OCCURS     4.
       05  FILLER                PIC  X(01)  VALUE  SPACE.
       05  HD4-N1                PIC  N(01).
       05  HD4-N2                PIC  N(01).
       05  HD4-N3                PIC  N(01).
       05  HD4-01                PIC  N(03).
       05  HD4-N4                PIC  N(01).
       05  HD4-02                PIC  N(03).
*******05  FILLER                PIC  X(04)  VALUE  SPACE.
       05  FILLER                PIC  X(03)  VALUE  SPACE.
*
 01  HD5.
     03  HD5-A                   OCCURS     4.
       05  FILLER                PIC  X(01)  VALUE  SPACE.
       05  HD5-N1                PIC  X(06).
       05  HD5-N2                PIC  N(01)
                                 CHARACTER  TYPE  IS  CHR-2.
       05  FILLER                PIC  X(04)  VALUE  SPACE.
       05  HD5-N3                PIC  N(10)
                                 CHARACTER  TYPE  IS  CHR-15.
*******05  FILLER                PIC  X(07)  VALUE  SPACE.
       05  FILLER                PIC  X(06)  VALUE  SPACE.
*
 01  HD6                         CHARACTER  TYPE  IS  CHR-2.
     03  HD6-A                   OCCURS     4.
       05  FILLER                PIC  X(01)  VALUE  SPACE.
       05  HD6-N1                PIC  N(03).
       05  HD6-N2                PIC  X(03).
       05  HD6-N3                PIC  N(01).
       05  FILLER                PIC  X(01)  VALUE  SPACE.
       05  HD6-N4                PIC  X(15).
*******05  FILLER                PIC  X(07)  VALUE  SPACE.
       05  FILLER                PIC  X(06)  VALUE  SPACE.
*
 01  HD7                         CHARACTER  TYPE  IS  CHR-2.
     03  HD7-A                   OCCURS     4.
       05  FILLER                PIC  X(01)  VALUE  SPACE.
       05  HD7-N1                PIC  N(03).
       05  HD7-N2                PIC  X(03).
       05  HD7-N3                PIC  N(01).
       05  FILLER                PIC  X(01)  VALUE  SPACE.
       05  HD7-N4                PIC  X(15).
*******05  FILLER                PIC  X(07)  VALUE  SPACE.
       05  FILLER                PIC  X(06)  VALUE  SPACE.
*
 01  HD8                         CHARACTER  TYPE  IS  CHR-15.
     03  HD8-A                   OCCURS     4.
       05  FILLER                PIC  X(01)  VALUE  SPACE.
       05  HD8-01                PIC  N(08).
       05  FILLER                PIC  X(01)  VALUE  SPACE.
       05  HD8-02                PIC  N(12)  VALUE  SPACE.
*******05  FILLER                PIC  X(03)  VALUE  SPACE.
       05  FILLER                PIC  X(01)  VALUE  SPACE.
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
     USE AFTER     EXCEPTION PROCEDURE      DDCENTF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### NSY0615L DKC ERROR " DKC-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     4000           TO        PROGRAM-STATUS.
     STOP     RUN.
 LBLERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      LBLXXXF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### NSY0615L LBL ERROR " LBL-ST " "
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
     DISPLAY  "### NSY0615L TOK ERROR " TOK-ST " "
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
     DISPLAY  "### NSY0615L TEN ERROR " TEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     4000           TO        PROGRAM-STATUS.
     STOP     RUN.
 PRTERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      PRTFILE.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### NSY0615L PRTFILE ERROR " PRT-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     4000           TO        PROGRAM-STATUS.
     STOP     RUN.
 HSBERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DCMHSBL2.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### NSY0615L DCMHSBL2 ERROR " HSB-ST " "
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
     DISPLAY  "*** NSY0615L START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     DDCENTF   HTOKMS    TENMS1.
     OPEN     OUTPUT    PRTFILE.
     OPEN     OUTPUT    LBLXXXF.
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
     START    DDCENTF   KEY  >=   DKC-F01   DKC-F02
                                  DKC-F03   DKC-F04
                                  DKC-F05   DKC-F06
                                  DKC-F18   DKC-F19
                                  DKC-F07
         INVALID   KEY
              MOVE    "END"  TO   DKC-FLG  END-FLG
              GO   TO   100-INIT-RTN-EXIT
     END-START.
*    ラベルデータ作成
     DISPLAY "DKC-FLG = " DKC-FLG UPON CONS.
     PERFORM UNTIL      DKC-FLG   =   "END"
*            センター納品データ読込み
             PERFORM DDCENTF-READ-SEC
             IF      DKC-FLG    =    SPACE
*            ラベルデータ出力
                     PERFORM    LBLXXXF-WT-SEC
             END-IF
     END-PERFORM.
     CLOSE   LBLXXXF.
     OPEN    INPUT   LBLXXXF.
*    ラベルデータ読込み
     PERFORM  LBLXXXF-READ-SEC.
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
     MOVE NC"お届け先"                     TO  HD0-01(IY).
     MOVE ":"                              TO  HD0-02(IY).
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
         WHEN 4
              MOVE      NC"大カゴ"     TO   HD2-01(IY)
     END-EVALUATE.
*    定番／特売区分
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
*#2019/03/18 NAV ST 発注種別変換マスタより取得
     MOVE     LBL-F03                      TO   HSB-F01.
     MOVE     LBL-F11                      TO   HSB-F03.
     PERFORM  DCMHSBL2-READ-SEC.
     IF  DCMHSBL2-INV-FLG  =  SPACE
              MOVE   HSB-F07               TO   HD8-01(IY)
     ELSE
              MOVE   NC"　　定　期　　"    TO   HD8-01(IY)
     END-IF.
*#2019/03/18 NAV ED 発注種別変換マスタより取得
*店舗名を表示
     MOVE     LBL-F03                       TO   TEN-F52.
     MOVE     LBL-F06                       TO   TEN-F011.
     PERFORM  900-TEN-READ.
     MOVE     TEN-F03                       TO   HD0-03(IY).
*
     IF       IY   =    4
*        出力
         WRITE     PRT-REC      FROM   HD0       AFTER  1
         WRITE     PRT-REC      FROM   HD1       AFTER  2
         WRITE     PRT-REC      FROM   HD2       AFTER  1
         WRITE     PRT-REC      FROM   HD3       AFTER  1
         WRITE     PRT-REC      FROM   HD4       AFTER  1
         WRITE     PRT-REC      FROM   HD5       AFTER  1
*********WRITE     PRT-REC      FROM   HD6       AFTER  1
         WRITE     PRT-REC      FROM   HD7       AFTER  1
         WRITE     PRT-REC      FROM   HD8       AFTER  1
         MOVE      SPACE        TO     PRT-REC
         WRITE     PRT-REC                       AFTER  1
         ADD       1            TO     LINE-CNT
         MOVE      ZERO         TO     IY
*
         PERFORM   VARYING  IX  FROM   1  BY  1  UNTIL  IX >= 4
              MOVE SPACE    TO    HD0-01(IX)
              MOVE SPACE    TO    HD0-02(IX)
              MOVE SPACE    TO    HD1-01(IX)
              MOVE SPACE    TO    HD2-01(IX)
              MOVE SPACE    TO    HD3-01(IX)
              MOVE SPACE    TO    HD4-01(IX)
              MOVE SPACE    TO    HD4-02(IX)
         END-PERFORM
         MOVE SPACE          TO   HD1  HD2  HD3  HD4  HD5  HD6
                                  HD7  HD8  HD0
     END-IF.
*
*    ラベルデータ読込み
     PERFORM LBLXXXF-READ-SEC.
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
*        出力
         WRITE     PRT-REC      FROM   HD0       AFTER  1
         WRITE     PRT-REC      FROM   HD1       AFTER  2
         WRITE     PRT-REC      FROM   HD2       AFTER  1
         WRITE     PRT-REC      FROM   HD3       AFTER  1
         WRITE     PRT-REC      FROM   HD4       AFTER  1
         WRITE     PRT-REC      FROM   HD5       AFTER  1
*********WRITE     PRT-REC      FROM   HD6       AFTER  1
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
     CLOSE    DDCENTF HTOKMS TENMS1 PRTFILE LBLXXXF.
*#2019/03/18 NAV ST
     CLOSE    DCMHSBL2.
*#2019/03/18 NAV ED
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** NSY0615L END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  センター納品データ読込み                    *
*--------------------------------------------------------------*
 DDCENTF-READ-SEC            SECTION.
     READ   DDCENTF     NEXT
         AT END
            MOVE  "END"  TO  DKC-FLG
            GO           TO  DDCENTF-READ-EXIT
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
              GO                 TO   DDCENTF-READ-EXIT
     END-IF.
     IF       PARA-SOKO   =  SPACE
              GO                 TO   DDCENTF-READ-0010
     END-IF.
*    抽出条件のチェック
     IF       PARA-SOKO      =   DKC-F04
              GO                 TO   DDCENTF-READ-0010
     ELSE
              GO                 TO   DDCENTF-READ-SEC
     END-IF.
 DDCENTF-READ-0010.
*    部門のチェック
***  DISPLAY "PARA-BUMON = " PARA-BUMON UPON CONS.
     IF       PARA-BUMON     =   999
*****OR       PARA-BUMON     =   SPACE
              CONTINUE
     ELSE
         IF   PARA-BUMON     =   DKC-F07
              CONTINUE
         ELSE
              GO                 TO   DDCENTF-READ-SEC
         END-IF
     END-IF.
*
 DDCENTF-READ-0020.
     IF  PARA-STDATE   <=   DKC-F05
     AND PARA-EDDATE   >=   DKC-F05
         CONTINUE
     ELSE
              GO                 TO   DDCENTF-READ-SEC
     END-IF.
*
 DDCENTF-READ-0030.
     IF  PARA-STTEN    <=   DKC-F06
     AND PARA-EDTEN    >=   DKC-F06
         CONTINUE
     ELSE
              GO                 TO   DDCENTF-READ-SEC
     END-IF.
*
 DDCENTF-READ-0040.
*****出力枚数が全て０の場合は対象外とする。
     IF  DKC-F08        =   ZERO
     AND DKC-F09        =   ZERO
     AND DKC-F10        =   ZERO
     AND DKC-F11        =   ZERO
              GO                 TO   DDCENTF-READ-SEC
     END-IF.
 DDCENTF-READ-0050.
*****出力枚数が全て０の場合は対象外とする。
     IF  DKC-F06    NOT =   DKC-F18
              GO                 TO   DDCENTF-READ-SEC
     END-IF.
*
 DDCENTF-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  ラベルデータ出力　　　　　                  *
*--------------------------------------------------------------*
 LBLXXXF-WT-SEC              SECTION.
*    小物
 LBLXXXF-WT-010.
     IF   DKC-F08  =  ZERO
          GO                 TO   LBLXXXF-WT-020
     END-IF.
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
 LBLXXXF-WT-020.
     IF   DKC-F09  =  ZERO
          GO                 TO   LBLXXXF-WT-030
     END-IF.
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
 LBLXXXF-WT-030.
     IF   DKC-F10  =  ZERO
          GO                 TO   LBLXXXF-WT-040
     END-IF.
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
 LBLXXXF-WT-040.
     IF   DKC-F11  =  ZERO
          GO                 TO   LBLXXXF-WT-EXIT
     END-IF.
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
 LBLXXXF-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  ラベルデータ読込み　　　                    *
*--------------------------------------------------------------*
 LBLXXXF-READ-SEC            SECTION.
     READ   LBLXXXF      NEXT
         AT END
            MOVE  "END"  TO  END-FLG
            GO           TO  LBLXXXF-READ-EXIT
         NOT   AT  END
            ADD    1     TO  LBL-CNT
     END-READ.
*
 LBLXXXF-READ-EXIT.
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
*#2019/03/18 NAV ST
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
*#2019/03/18 NAV ED
