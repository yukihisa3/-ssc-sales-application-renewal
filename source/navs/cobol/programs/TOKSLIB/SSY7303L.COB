****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　出荷明細書（ダイユーエイト）      *
*    モジュール名　　　　：　出荷明細書作成　　　　　　　　　　*
*    作成日／更新日　　　：　05/07/25                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　仕入ﾃﾞｰﾀﾌｧｲﾙﾌｧｲﾙより支払明細書を
*                        ：　印刷する。　　　　　　　　　　　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY7303L.
 AUTHOR.                N.KANEKO.
 DATE-WRITTEN.          00/11/07.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         YA        IS   YA
         YB        IS   PITCH-15
         YB-21     IS   BAIKAKU-15
         YA-21     IS   BAIKAKU
         YA-22     IS   BAIKAKU-22
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< サカタ出荷ﾃﾞｰﾀﾌｧｲﾙ >>--*
     SELECT   DYSHIRED  ASSIGN         DA-01-S-DYSHIRED
                        ORGANIZATION   SEQUENTIAL
                        STATUS         DEN-ST.
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS    ASSIGN         DA-01-VI-TOKMS2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TOK-F01
                        STATUS         TOK-ST.
*----<< 店舗マスタ >>--*
     SELECT   HTENMS    ASSIGN         DA-01-VI-TENMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TEN-F52   TEN-F011
                        STATUS         TEN-ST.
*----<< プリンタ >>-*
     SELECT   PRTF      ASSIGN         LP-04.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 仕入ﾃﾞｰﾀファイル >>--*
 FD  DYSHIRED           LABEL RECORD   IS   STANDARD
                        BLOCK CONTAINS  1  RECORDS.
     COPY     DYSHIRED  OF        XFDLIB
              JOINING   DEN       PREFIX.
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< 店舗マスタ >>--*
 FD  HTENMS             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*----<< プリンタ >>-*
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC            PIC  X(200).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  COUNTERS.
     03  LINE-CNT       PIC  9(03).
     03  PAGE-CNT       PIC  9(03).
     03  CNT-READ       PIC  9(03).
 01  IDX.
     03  I              PIC  9(03).
 01  ID-PROGRAM.
     03  PG-ID          PIC  X(08)     VALUE  "SSY7303L".
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  DEN-ST             PIC  X(02).
 01  TOK-ST             PIC  X(02).
 01  TEN-ST             PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
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
 01  WK-DATE            PIC  9(06).
 01  FILLER             REDEFINES      WK-DATE.
     03  WK-YY          PIC  9(02).
     03  WK-MM          PIC  9(02).
     03  WK-DD          PIC  9(02).
*
*----<< BREAK KEY >>--*
 01  BREAK-KEY.
     03  NEW-VALUE.
         05  NEW-TENCD  PIC  9(05).
         05  NEW-NOUHD  PIC  9(08).
         05  NEW-TYUNO  PIC  9(08).
     03  OLD-VALUE.
         05  OLD-TENCD  PIC  9(05).
         05  OLD-NOUHD  PIC  9(08).
         05  OLD-TYUNO  PIC  9(08).
*WK店舗番号
 01  WK-TENNO           PIC  9(05).
*WK店舗名
 01  WK-TEN             PIC  N(15).
*WK納品指定日
 01  WK-NOU             PIC  9(08).
*WK受注日
 01  WK-JUN             PIC  9(06).
*表示用フラグ
 01  HEN-FLG            PIC  9.
*欠品数
 01  WK-KETU            PIC  9(05).
*
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
******************    HEADER    **************************
*
*一行目
 01  HEAD01.
     03  FILLER         CHARACTER TYPE BAIKAKU-22.
         05  FILLER     PIC  X(56)     VALUE  SPACE.
         05  FILLER     PIC  N(09)     VALUE
             NC"出　荷　明　細　書".
*    処理日
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER     PIC  X(20)     VALUE  SPACE.
         05  HD-011     PIC  9(02).
         05  FILLER     PIC  N(01)     VALUE  NC"年".
         05  HD-012     PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"月".
         05  HD-013     PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"日".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
*    ページ数
         05  HD-014      PIC  ZZ9.
         05  FILLER     PIC  N(01)     VALUE  NC"頁".
*二行目
 01  HEAD02.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"受注日".
         05  FILLER     PIC  X(09)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"納品指定日".
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"出荷指示日".
*三行目
 01  HEAD03.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER     PIC  X(03)     VALUE  SPACE.
*    受注日
         05  HD-021     PIC  99.
         05  FILLER     PIC  N(01)     VALUE  NC"年".
         05  HD-022     PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"月".
         05  HD-023     PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"日".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
*    納品指定日
         05  HD-024     PIC  99.
         05  FILLER     PIC  N(01)     VALUE  NC"年".
         05  HD-025     PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"月".
         05  HD-026     PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"日".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
*    出荷指示日
         05  HD-027     PIC  99.
         05  FILLER     PIC  N(01)     VALUE  NC"年".
         05  HD-028     PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"月".
         05  HD-029     PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"日".
*四行目
 01  HEAD04         CHARACTER TYPE YA.
     03  FILLER.
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"店名".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
*    店番
         05  HD-031     PIC  9(05).
         05  FILLER     PIC  X(03)     VALUE  SPACE.
*    店名
         05  HD-032     PIC  N(15).
*五行目
 01  HEAD05.
     03  FILLER         CHARACTER TYPE YA.
*    各項目
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"注文ＮＯ".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(06)     VALUE  NC"ＪＡＮコード".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"商品名".
         05  FILLER     PIC  X(28)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"受注数".
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"出荷数".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"備考".
*    点線
 01  SEN.
     03  FILLER         OCCURS  136.
         05  FILLER     PIC  X(01)     VALUE  "-".
*
******************    MEISAI    **************************
*
*明細一行目
 01  MEIS01.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER     PIC  X(03)     VALUE  SPACE.
*    注文ＮＯ
         05  ME-01      PIC  9(08).
         05  FILLER     PIC  X(03)     VALUE  SPACE.
*    ＪＡＮコード
         05  ME-02      PIC  9(13).
         05  FILLER     PIC  X(03)     VALUE  SPACE.
*    商品名
         05  ME-03      PIC  X(30).
         05  FILLER     PIC  X(03)     VALUE  SPACE.
*    受注数
         05  ME-04      PIC  ZZZZ9.99.
         05  FILLER     PIC  X(03)     VALUE  SPACE.
*    出荷数
         05  ME-05      PIC  ZZZZ9.99.
         05  FILLER     PIC  X(03)     VALUE  SPACE.
*    備考
         05  ME-06      PIC  N(020).
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 仕入データ >>--*
 SHI-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DYSHIRED.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID "  DYSHIRED  ERROR " DEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID "  HTOKMS   ERROR " TOK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 店舗マスタ >>--*
 HTENMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTENMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID "  HTENMS   ERROR " TEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
*スタートメッセージ
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "***   " PG-ID " START  *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     NEW-VALUE  =    HIGH-VALUE.
     PERFORM  300-END-RTN.
*
*終了メッセージ出力
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "***   " PG-ID " END    *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
*ファイルのオープン
     OPEN     INPUT     DYSHIRED.
     OPEN     INPUT     HTOKMS.
     OPEN     INPUT     HTENMS.
     OPEN     OUTPUT    PRTF.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         COUNTERS.
     MOVE     99             TO   LINE-CNT.
     MOVE     LOW-VALUE      TO   BREAK-KEY.
*仕入データ初期読込み
     PERFORM  900-DEN-READ.
     MOVE     NEW-VALUE      TO   OLD-VALUE.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*
     IF       NEW-VALUE       NOT =     OLD-VALUE
*             キーの入れ替え
              MOVE     NEW-VALUE            TO   OLD-VALUE
              MOVE     1                    TO   HEN-FLG
     END-IF.
*    明細
     PERFORM   210-MEIS01-PRINT
*
     PERFORM   900-DEN-READ.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
*ファイルのクローズ
     CLOSE    DYSHIRED.
     CLOSE    HTOKMS.
     CLOSE    HTENMS.
     CLOSE    PRTF.
*
     DISPLAY "* SITGKFG (IN)=" CNT-READ " *" UPON CONS.
     DISPLAY "* PRINTF(PAGE)=" PAGE-CNT " *" UPON CONS.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3     ﾒｲｻｲ ｲﾝｻﾂ（明細行出力）                      *
*--------------------------------------------------------------*
 210-MEIS01-PRINT       SECTION.
     IF       LINE-CNT  >    60
              PERFORM   211-HEAD-PRINT
     END-IF.
*
     MOVE     SPACE          TO   MEIS01.
*    注文ＮＯ
     IF       HEN-FLG  = 1
              MOVE     DEN-A03        TO   ME-01
     END-IF.
*    ＪＡＮコード(指定商品コード)
     MOVE     DEN-F25        TO   ME-02.
*    商品名
     MOVE     DEN-A34        TO   ME-03.
*    受注数
     MOVE     DEN-F50        TO   ME-04.
*    出荷数
     MOVE     DEN-F15        TO   ME-05.
*    備考
*    欠品数の計算
     COMPUTE  WK-KETU  =  DEN-F50 - DEN-F15.
     EVALUATE   WK-KETU
        WHEN    0
        MOVE    SPACE              TO    ME-06
        WHEN    DEN-F50
        MOVE    NC"全欠品です。"   TO    ME-06
        WHEN    OTHER
        MOVE    NC"欠品有です。"   TO    ME-06
     END-EVALUATE.
*
     WRITE    PRT-REC   FROM MEIS01    AFTER     1.
     IF       NEW-VALUE     =    OLD-VALUE
         ADD  1              TO   LINE-CNT
     ELSE
         MOVE 99             TO   LINE-CNT
     END-IF.

     MOVE     0              TO   HEN-FLG.
*
 210-MEIS01-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL 4       ﾀｲﾄﾙ ﾌﾟﾘﾝﾄ ｼｮﾘ（ＨＥＡＤプリント）          *
*--------------------------------------------------------------*
 211-HEAD-PRINT         SECTION.
*
*    改頁制御
     IF       PAGE-CNT  >    0
              MOVE      SPACE     TO   PRT-REC
              WRITE     PRT-REC   AFTER  PAGE
              MOVE      ZERO      TO   LINE-CNT
     END-IF.
*    ページカウンター
     ADD      1                   TO   PAGE-CNT.
*    システム日付
     MOVE     SYS-YY              TO   HD-011.
     MOVE     SYS-MM              TO   HD-012.
     MOVE     SYS-DD              TO   HD-013.
*    頁
     MOVE     PAGE-CNT            TO   HD-014.
*    受注日
     MOVE     WK-JUN(1:2)         TO   HD-021.
     MOVE     WK-JUN(3:2)         TO   HD-022.
     MOVE     WK-JUN(5:2)         TO   HD-023.
*    納品指定日
     MOVE     WK-NOU(3:2)         TO   HD-024.
     MOVE     WK-NOU(5:2)         TO   HD-025.
     MOVE     WK-NOU(7:2)         TO   HD-026.
*    出荷指示日(ｼｽﾃﾑ日付)
     MOVE     SYS-YY              TO   HD-027.
     MOVE     SYS-MM              TO   HD-028.
     MOVE     SYS-DD              TO   HD-029.
*    店舗名
     MOVE     WK-TENNO            TO   HD-031.
     MOVE     WK-TEN              TO   HD-032.
*    ＨＥＡＤ出力
     WRITE    PRT-REC   FROM      HEAD01    AFTER     2.
     WRITE    PRT-REC   FROM      HEAD02    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD03    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD04    AFTER     2.
     WRITE    PRT-REC   FROM      SEN       AFTER     1.
     WRITE    PRT-REC   FROM      HEAD05    AFTER     1.
     WRITE    PRT-REC   FROM      SEN       AFTER     1.
     MOVE     9         TO        LINE-CNT.
*
 211-HEAD-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    取引先マスタ　 READ                          *
*--------------------------------------------------------------*
 900-TOK-READ           SECTION.
     READ     HTOKMS    INVALID
              MOVE      SPACE          TO   TOK-F02
     END-READ.
 900-TOK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    店舗マスタ　　READ                           *
*--------------------------------------------------------------*
 900-TEN-READ           SECTION.
     READ     HTENMS    INVALID
              MOVE      SPACE          TO   TEN-F03
     END-READ.
 900-TEN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    仕入データ　　READ                           *
*--------------------------------------------------------------*
 900-DEN-READ           SECTION.
     READ     DYSHIRED  AT   END
              MOVE      HIGH-VALUE     TO   NEW-VALUE
              GO   TO   900-DEN-READ-EXIT
     END-READ.
     IF       DEN-A18  NOT =  1
              GO                 TO   900-DEN-READ
     END-IF.
         MOVE     DEN-F07        TO   NEW-TENCD
                                      WK-TENNO.

*    店舗名取得
     MOVE     1225           TO   TEN-F52.
     MOVE     DEN-F07        TO   TEN-F011.
     PERFORM  900-TEN-READ.
     IF       TEN-F03   =   SPACE
              IF   DEN-F07 NOT = ZERO
                   MOVE ALL NC"＊"     TO   WK-TEN
              END-IF
     ELSE
              MOVE      TEN-F03        TO   WK-TEN
     END-IF.
         MOVE     DEN-A08        TO   WK-JUN.
         MOVE     DEN-F112       TO   NEW-NOUHD
                                      WK-NOU.
         MOVE     DEN-A03        TO   NEW-TYUNO.
     IF  NEW-TENCD   NOT =  OLD-TENCD
     OR  NEW-NOUHD   NOT =  OLD-NOUHD
                  PERFORM   211-HEAD-PRINT
     END-IF.
     ADD      1      TO   CNT-READ.
*
 900-DEN-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
