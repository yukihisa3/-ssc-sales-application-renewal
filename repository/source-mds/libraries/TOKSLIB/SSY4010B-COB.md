# SSY4010B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SSY4010B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    サブシステム　　　　：　出荷管理システム                  *
*    業務名　　　　　　　：　オンライン配送発注集計表　　　　　*
*    モジュール名　　　　：　オンライン配送発注集計データ抽出　*
*    作成日／更新日　　　：　2000/03/01                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　                                  *
*                        ：                                    *
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY4010B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2000/03/01.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*売上伝票ファイル
     SELECT  SHTDENF   ASSIGN    TO    SHTDENLA
                       ORGANIZATION    SEQUENTIAL
                       STATUS          DEN-ST.
*商品コード変換ファイル
     SELECT   HSHOTBL  ASSIGN          SHOTBL1
                       ORGANIZATION    INDEXED
                       ACCESS    MODE  RANDOM
                       RECORD    KEY   SHO-F01  SHO-F02
                       STATUS          SHO-ST.
*配送発注集計表ワークファイル
     SELECT   SHWHISF  ASSIGN          SHWHISF
                       ORGANIZATION    SEQUENTIAL
                       STATUS          HIS-ST.
*店舗別ルートマスタ
     SELECT   HTENRMS  ASSIGN    TO    TENRMS1
                       ORGANIZATION    INDEXED
                       ACCESS    MODE  RANDOM
                       RECORD    KEY   TER-F01 TER-F02
                                       TER-F03
                       STATUS          TER-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
****************************************************************
*    FILE = 売上伝票ファイル                                   *
****************************************************************
 FD  SHTDENF            BLOCK CONTAINS   1  RECORDS.
 01  DEN-REC.
     03  DEN1.
         05  DEN1-01              PIC       9(08).
         05  DEN1-02              PIC       9(09).
         05  DEN1-03              PIC       9(02).
         05  DEN1-04              PIC       9(01).
         05  DEN1-05              PIC       X(12).
         05  DEN1-06              PIC       9(05).
         05  DEN1-07              PIC       X(02).
         05  DEN1-08              PIC       X(02).
         05  DEN1-09              PIC       X(28).
         05  DEN1-10              PIC       X(02).
         05  DEN1-11              PIC       X(02).
     03  DEN2.
         05  DEN2-01              PIC       X(08).
         05  DEN2-02              PIC       X(75).
         05  DEN2-03              PIC      S9(09) PACKED-DECIMAL.
         05  DEN2-04              PIC      S9(09) PACKED-DECIMAL.
         05  DEN2-05              PIC       X(32).
     03  DEN3                     PIC       X(13).
     03  DEN4.
         05  DEN4-01              PIC       X(119).
         05  DEN4-02              PIC       9(08).
         05  DEN4-03              PIC       9(04).
         05  DEN4-04              PIC       X(02).
         05  DEN4-05              PIC       X(46).
         05  DEN4-06              PIC       9(04).
         05  DEN4-07              PIC       9(03).
         05  DEN4-08              PIC       X(623).
****************************************************************
*    FILE = 商品コード変換テーブル                             *
****************************************************************
 FD  HSHOTBL            LABEL     RECORD   IS   STANDARD.
                        COPY      HSHOTBL  OF   XFDLIB
                        JOINING   SHO      AS   PREFIX.
****************************************************************
*    FILE = 配送発注集計表ワークファイル                       *
****************************************************************
*FD  SHWHISF            LABEL     RECORD   IS   STANDARD.
**************          BLOCK     CONTAINS 27   RECORDS.
*                       COPY      SHWHISF  OF   XFDLIB
*                       JOINING   HIS      AS   PREFIX.
 FD  SHWHISF            BLOCK CONTAINS   1  RECORDS.
 01  HIS-REC.
     03  HIS1.
         05  HIS1-01              PIC       X(37).
         05  HIS1-02              PIC       X(02).
         05  HIS1-03              PIC       X(34).
     03  HIS2.
         05  HIS2-01              PIC       X(08).
         05  HIS2-02              PIC       X(75).
         05  HIS2-03              PIC      S9(09) PACKED-DECIMAL.
         05  HIS2-04              PIC      S9(09) PACKED-DECIMAL.
         05  HIS2-05              PIC       X(32).
     03  HIS3                     PIC       X(13).
     03  HIS4                     PIC       X(41).
     03  HIS5                     PIC       9(02).
     03  HIS6                     PIC       9(02).
****************************************************************
*    FILE = 店舗別ルートマスタ                                 *
****************************************************************
 FD  HTENRMS            LABEL     RECORD   IS   STANDARD.
                        COPY      HTENRMS  OF   XFDLIB
                        JOINING   TER      AS   PREFIX.
****************************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  DEN-ST         PIC  X(02).
     03  SHO-ST         PIC  X(02).
     03  HIS-ST         PIC  X(02).
     03  TER-ST         PIC  X(02).
*フラグ領域
 01  FLG-AREA.
     03  END-FLG        PIC  X(03).
*ワーク領域
 01  WRK-AREA.
     03  WK-NONYU-DATE  PIC  9(08).
*プログラムＩＤ
 01  PG-ID.
     03  ID-PG          PIC  X(08)     VALUE  "SSY4010B".
*日付ワーク
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
*エラーセクション名
 01  SEC-NAME.
     03  FILLER         PIC  X(05)  VALUE " *** ".
     03  S-NAME         PIC  X(30).
*
*カウント領域
 01  CNT-AREA.
     03  CNT-READ            PIC  9(08).
     03  CNT-WRITE           PIC  9(08).
*
******************************************************************
 LINKAGE           SECTION.
******************************************************************
 01  BATNO.
     03  LINK-DATE           PIC  9(08).
     03  LINK-TIME           PIC  9(04).
     03  LINK-TORICD         PIC  X(08).
 01  SOKOCD.
     03  LINK-SOKO-KAISI     PIC  X(02).
     03  LINK-SOKO-OWARI     PIC  X(02).
*
****************************************************************
 PROCEDURE              DIVISION  USING  BATNO  SOKOCD.
****************************************************************
****************************************************************
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
****************************************************************
 DECLARATIVES.
*売上伝票データ
 DEN-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHTDENF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  SEC-NAME       UPON CONS.
     DISPLAY  "### " PG-ID " SHTDENF ERROR " DEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     STOP     RUN.
*商品コード変換テーブル
 SHO-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HSHOTBL.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  SEC-NAME       UPON CONS.
     DISPLAY  "### " PG-ID " HSHOTBL ERROR " SHO-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     STOP     RUN.
*配送発注集計ワークファイル
 HIS-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHWHISF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  SEC-NAME       UPON CONS.
     DISPLAY  "### " PG-ID " SHWHISF ERROR " HIS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     STOP     RUN.
*店舗別ルートマスタ
 TER-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTENRMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  SEC-NAME       UPON CONS.
     DISPLAY  "### " PG-ID " HTENRMS ERROR " TER-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     STOP     RUN.
 END DECLARATIVES.
****************************************************************
*                       SHORI                        0.0       *
****************************************************************
 CONTROL-START          SECTION.
     MOVE     "CONTROL-START"     TO   S-NAME.
*開始メッセージ出力
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** " PG-ID " START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
******
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC   UNTIL     END-FLG  =  "END".
     PERFORM  END-SEC.
******
*終了メッセージ出力
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** " PG-ID " END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     STOP RUN.
*
 CONTROL-START-EXIT.
     EXIT.
****************************************************************
*    初期処理                                        1.0       *
****************************************************************
 INIT-SEC           SECTION.
     MOVE     "INIT-SEC"      TO   S-NAME.
*ファイルのオープン
     OPEN     INPUT     SHTDENF  HTENRMS
                        HSHOTBL
     OPEN     OUTPUT    SHWHISF.
*初期化
     INITIALIZE    FLG-AREA  WRK-AREA  CNT-AREA.
*
     PERFORM  FL-RD-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*    メイン処理                                      2.0       *
****************************************************************
 MAIN-SEC           SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*
*店舗別ルートマスタの読込み
***  取引先コード
     MOVE     DEN1-01        TO   TER-F01.
***  振分倉庫コード
     MOVE     DEN1-07        TO   TER-F02.
***  店舗コード
     MOVE     DEN1-06        TO   TER-F03.
     READ     HTENRMS
     INVALID
         DISPLAY   "##ﾙｰﾄﾅｼ##" TER-F01 "-" TER-F02 "-" TER-F03
                   UPON CONS
         GO    TO            MAIN-010
     END-READ.
*    配送発注集計表ワークファイル出力
     PERFORM  HIS-WRITE-SEC.
 MAIN-010.
*    売上伝票データＲＥＡＤ
     PERFORM  FL-RD-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*    配送発注集計表ワークファイル出力　              2.1       *
****************************************************************
 HIS-WRITE-SEC          SECTION.
     MOVE     "HIS-WRITE-SEC"     TO   S-NAME.
*
*レコード初期化
     MOVE     SPACE          TO   HIS-REC.
     INITIALIZE                   HIS-REC.
*項目出力
***  取引先コード～伝発区分
     MOVE     DEN1           TO   HIS1.
***  サカタコード
***  商品変換テーブルＲＥＡＤ（売上伝票Ｆに自社商品コード無時）
     IF       DEN2-01        =    SPACE
***        取引先コード，指定商品コードで商品変換テーブルREAD
              MOVE     DEN1-01        TO   SHO-F01
              MOVE     DEN3           TO   SHO-F02
              PERFORM  SHO-READ-SEC
***        自社商品コード
              MOVE     SHO-F031       TO   HIS2-01
              MOVE     DEN2-02        TO   HIS2-02
*** （売上伝票Ｆに自社商品コード有時）
     ELSE
              MOVE     DEN2-01        TO   HIS2-01
              MOVE     DEN2-02        TO   HIS2-02
     END-IF.
***  指定商品コード
     MOVE     DEN3           TO   HIS3.
***  出荷場所
     MOVE     DEN1-07        TO   HIS1-02.
***  ルート
     MOVE     TER-F04        TO   HIS5.
***  ルート順
     MOVE     TER-F05        TO   HIS6.
***  セリ番号／セリＳＥＱセット　項目は、消費税、粗利欄を使用
     MOVE     DEN4-06        TO   HIS2-03.
     MOVE     DEN4-07        TO   HIS2-04.
*    出力
     WRITE    HIS-REC.
*    ADD      1              TO   CNT-WRITE.
*
 HIS-WRITE-EXIT.
     EXIT.
****************************************************************
*    売上伝票データ　　 READ                                   *
****************************************************************
 FL-RD-SEC           SECTION.
     MOVE     "FL-RD-SEC"      TO   S-NAME.
*
     READ     SHTDENF
         AT END
              MOVE     "END"      TO   END-FLG
              GO        TO        FL-RD-EXIT
     END-READ.
*備考行が対象外
     IF       DEN2-01  =  "99      "
              GO        TO        FL-RD-SEC
     END-IF.
*
 FL-RD-EXIT.
     EXIT.
****************************************************************
*                 商品コード変換テーブル　READ                 *
****************************************************************
 SHO-READ-SEC           SECTION.
     MOVE     "SHO-READ-SEC"      TO   S-NAME.
*
     READ     HSHOTBL
         INVALID
         DISPLAY "商品テーブルエラー　取引先　＝　" SHO-F01
               "　店舗　＝　"  DEN1-06
               "　商品　＝　"  SHO-F02
              MOVE      SPACE     TO   SHO-F031
              MOVE      ZERO      TO   SHO-F04
     END-READ.
*
 SHO-READ-EXIT.
     EXIT.
****************************************************************
*    終了処理                                        3.0       *
****************************************************************
 END-SEC            SECTION.
     MOVE     "END-SEC"       TO   S-NAME.
*
*ファイルのクローズ
     CLOSE    SHTDENF
              HSHOTBL
              HTENRMS
              SHWHISF.
*
 END-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
