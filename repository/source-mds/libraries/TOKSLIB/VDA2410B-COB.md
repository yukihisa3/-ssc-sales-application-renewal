# VDA2410B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/VDA2410B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：（株）サカタのタネ殿　　　　　　　　*
*    業務名　　　　　　　：　カーマオンラインシステム　　　　　*
*    モジュール名　　　　：　伝票データフォーマット変換　　　　*
*    作成日／更新日　　　：　1999/02/04  1999/10/19            *
*    作成者／更新者　　　：　ＮＡＶ高橋　ＮＡＶ高橋　　　　　　*
*    処理概要　　　　　　：　受信データをサカタ専用フォーマット*
*    　　　　　　　　　　：　に変換する．（２０００年対応）　　*
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            VDA2410B.
 AUTHOR.                T.TAKAHASHI.
 DATE-WRITTEN.          99/02/04.
*REMARKS.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM.
 OBJECT-COMPUTER.       FACOM.
 SPECIAL-NAMES.         CONSOLE   IS   CONS
                        YA        IS   YA
                        YB-21     IS   YB-21.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*****<< 分割済ファイル >>*****
     SELECT   INFILE    ASSIGN    TO        INFILE
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   IN-STATUS.
*
*****<<伝票データＦ     >>*****
     SELECT   KMSHIRED  ASSIGN    TO        KMSHIRED
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   KM-STATUS.
*****<<プリント　ファイル>>****
     SELECT   PRTF      ASSIGN    TO        LP-04
                        FILE      STATUS    IS   PR-STATUS.
*-----------------------------------------------------------------
 DATA                   DIVISION.
*-----------------------------------------------------------------
 FILE                   SECTION.
*
*****<< 分割済ファイル >>*****
 FD  INFILE             BLOCK CONTAINS   1  RECORDS.
*＜伝票ヘッダ＞
 01  DH-REC.
     03  DH01                     PIC       X(01).
     03  DH02                     PIC       9(02).
     03  DH03                     PIC       9(05).
     03  DH04                     PIC       9(04).
     03  DH05                     PIC       9(03).
     03  DH06                     PIC       9(06).
     03  DH07                     PIC       X(03).
     03  DH08                     PIC       9(02).
     03  DH09                     PIC       9(02).
     03  DH10                     PIC       9(08).
     03  DH11                     PIC       9(08).
     03  DH12                     PIC       X(10).
     03  DH13                     PIC       X(07).
     03  DH14                     PIC       X(10).
     03  DH15                     PIC       X(15).
     03  DH16                     PIC       X(02).
     03  DH17                     PIC       X(08).
     03  DH18                     PIC       X(08).
     03  DH19                     PIC       X(05).
     03  DH20                     PIC       X(06).
     03  DH21                     PIC       X(01).
     03  DH22                     PIC       X(03).
     03  DH23                     PIC       X(09).
*＜伝票明細レコード＞
 01  DD-REC.
     03  DD01                     PIC       X(01).
     03  DD02                     PIC       9(02).
     03  DD03                     PIC       9(03).
     03  DD04                     PIC       9(06).
     03  DD05                     PIC       9(02).
     03  DD06                     PIC       9(07).
     03  DD07                     PIC       9(01).
     03  DD08                     PIC       X(20).
     03  DD09                     PIC       X(20).
     03  DD10                     PIC       X(13).
     03  DD11                     PIC       9(05)V9(01).
     03  DD12                     PIC       9(07)V9(02).
     03  DD13                     PIC       9(07).
     03  DD14                     PIC       X(31).
*
*****<<伝票データＦ     >>*****
 FD  KMSHIRED            BLOCK CONTAINS   3  RECORDS.
 01  KMS-REC.
     03  KMS01                    PIC       9(08).
     03  KMS02                    PIC       9(09).
     03  KMS03                    PIC       9(02).
     03  KMS04                    PIC       9(02).
     03  KMS05                    PIC       9(02).
     03  KMS06                    PIC       X(02).
     03  KMS07                    PIC       X(20).
     03  KMS08                    PIC       X(20).
     03  KMS09                    PIC       X(20).
     03  KMS10                    PIC       9(06).
     03  KMS11                    PIC       9(06).
     03  KMS12                    PIC       9(02).
     03  KMS13                    PIC       X(13).
     03  KMS14                    PIC       9(07).
     03  KMS15                    PIC       X(20).
     03  KMS16                    PIC       X(20).
     03  KMS17                    PIC       9(05)V9.
     03  KMS18                    PIC       9(07)V99.
     03  KMS19                    PIC       9(07).
     03  KMS20                    PIC       9(10).
     03  KMS21                    PIC       9(10).
     03  KMS22                    PIC       9(01).
     03  KMS23                    PIC       9(05).
     03  KMS24                    PIC       X(10).
     03  KMS25                    PIC       X(03).
     03  KMS26                    PIC       9(04).
     03  KMS27                    PIC       X(06).
     03  KMS28                    PIC       X(05).
     03  KMS29                    PIC       9(06).
     03  KMS30                    PIC       9(06).
     03  KMS31                    PIC       9(04).
     03  KMS32                    PIC       X(15).
     03  KMS33                    PIC       X(01).
     03  KMS34                    PIC       X(03).
     03  FILLER                   PIC       X(70).
****＜プリント　ファイル＞*****
 FD  PRTF.
 01  PRT-REC                      PIC       X(200).
*
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*----------------------------------------------------------------*
*    エンドフラグ
 01  END-FLG                PIC       X(03)  VALUE SPACE.
*    ステイタス　エリア
 01  STATUS-AREA.
     03  IN-STATUS          PIC       X(02).
     03  KM-STATUS          PIC       X(02).
     03  PR-STATUS          PIC       X(02).
*    伝票枚数カウント
 01  CNT-AREA.
     03  WK-DENCNT          PIC       9(04)  VALUE ZERO.
     03  WK-DENCNT1         PIC       9(04)  VALUE ZERO.
*    伝票_退避
 01  DEN-AREA.
     03  WK-DD10            PIC       9(07)  VALUE ZERO.
*    日付退避エリア
 01  WK-DATE.
     03  WK-YY              PIC       9(02)  VALUE ZERO.
     03  WK-YMD             PIC       9(06)  VALUE ZERO.
*    日付退避エリア
 01  WK-TIME                PIC       9(06)  VALUE ZERO.
*    日付６桁，日付８桁保存
 01  SYS-DATE6              PIC       9(06)  VALUE ZERO.
 01  SYS-DATE8              PIC       9(08)  VALUE ZERO.
 01  HEN-SYS.
     03  SYS-YYYY           PIC       9(04)  VALUE ZERO.
     03  SYS-MM             PIC       9(02)  VALUE ZERO.
     03  SYS-DD             PIC       9(02)  VALUE ZERO.
*    日付６桁分割
 01  HEN-DATE.
     03  HEN-YY             PIC       9(02)  VALUE ZERO.
     03  HEN-MM             PIC       9(02)  VALUE ZERO.
     03  HEN-DD             PIC       9(02)  VALUE ZERO.
*    ルート変換
 01  WK-RUTO.
     03  WK-RUTO-S          PIC       X(02).
     03  WK-RUTO-S-R        REDEFINES WK-RUTO-S.
         05  WK-RUTO-H      PIC       9(02).
*    メッセージ　エリア
 01  MSG-AREA.
     03  MSG-ABEND1.
         05  FILLER         PIC       X(04)  VALUE "### ".
         05  ERR-PG-ID      PIC       X(08)  VALUE "VDA2410B".
         05  FILLER         PIC       X(10)  VALUE " ABEND ###".
     03  MSG-ABEND2.
         05  FILLER         PIC       X(04)  VALUE "### ".
         05  ERR-FL-ID      PIC       X(08).
         05  FILLER         PIC       X(04)  VALUE " ST-".
         05  ERR-STCD       PIC       X(02).
         05  FILLER         PIC       X(04)  VALUE " ###".
*データ情報退避ワーク（ヘッダ部）
 01  WK-DH-REC.
     03  WK-DH01                     PIC       X(01).
     03  WK-DH02                     PIC       9(02).
     03  WK-DH03                     PIC       9(05).
     03  WK-DH04                     PIC       9(04).
     03  WK-DH05                     PIC       9(03).
     03  WK-DH06                     PIC       9(06).
     03  WK-DH07                     PIC       X(03).
     03  WK-DH08                     PIC       9(02).
     03  WK-DH09                     PIC       9(02).
     03  WK-DH10                     PIC       9(08).
     03  WK-DH11                     PIC       9(08).
     03  WK-DH12                     PIC       X(10).
     03  WK-DH13                     PIC       X(07).
     03  WK-DH14                     PIC       X(10).
     03  WK-DH15                     PIC       X(15).
     03  WK-DH16                     PIC       X(02).
     03  WK-DH17                     PIC       X(08).
     03  WK-DH18                     PIC       X(08).
     03  WK-DH19                     PIC       X(05).
     03  WK-DH20                     PIC       X(06).
     03  WK-DH21                     PIC       X(01).
     03  WK-DH22                     PIC       X(03).
     03  WK-DH23                     PIC       X(09).
*    見出し行１
 01  HD01.
     03  FILLER                   PIC       X(47)  VALUE SPACE.
     03  FILLER                   PIC       N(12)  VALUE
       NC"【　データ件数リスト　】"
       CHARACTER   TYPE   IS   YB-21.
     03  FILLER                   PIC       X(24)  VALUE SPACE.
     03  FILLER                   PIC       X(05)  VALUE "DATE:".
     03  HD01-YY                  PIC       9999.
     03  FILLER                   PIC       X(01)  VALUE ".".
     03  HD01-MM                  PIC       Z9.
     03  FILLER                   PIC       X(01)  VALUE ".".
     03  HD01-DD                  PIC       Z9.
     03  FILLER                   PIC       X(02)  VALUE SPACE.
     03  FILLER                   PIC       X(05)  VALUE "PAGE:".
     03  HD01-PAGE                PIC       ZZZ9.
*    明細行
 01  MD01               CHARACTER TYPE      IS     YA.
     03  FILLER                   PIC       X(11)  VALUE SPACE.
     03  MD01-USERNM              PIC       N(05).
     03  FILLER                   PIC       X(10)  VALUE SPACE.
     03  FILLER                   PIC       N(03)  VALUE
                                            NC"件数：".
     03  MD01-KENSU               PIC       ZZ,ZZ9.
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION           PROCEDURE INFILE.
     MOVE     "INFILE"            TO        ERR-FL-ID.
     MOVE     IN-STATUS           TO        ERR-STCD.
     MOVE     4000                TO        PROGRAM-STATUS.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC2         SECTION.
     USE AFTER          EXCEPTION           PROCEDURE KMSHIRED.
     MOVE     "KMSHIRED"          TO        ERR-FL-ID.
     MOVE     KM-STATUS           TO        ERR-STCD.
     MOVE     4000                TO        PROGRAM-STATUS.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC3         SECTION.
     USE AFTER          EXCEPTION           PROCEDURE PRTF.
     MOVE     "PRTF    "          TO        ERR-FL-ID.
     MOVE     PR-STATUS           TO        ERR-STCD.
     MOVE     4000                TO        PROGRAM-STATUS.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     STOP     RUN.
*
 END          DECLARATIVES.
*
******************************************************************
*    0.0      コントロール
******************************************************************
 CONTROL-START         SECTION.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC       UNTIL  END-FLG = "END".
     PERFORM  END-SEC.
     STOP     RUN.
 CONTROL-END.
     EXIT.
******************************************************************
*    1.0      初期処理
******************************************************************
 INIT-SEC              SECTION.
*    ファイルのオープン
     OPEN     INPUT     INFILE.
     OPEN     OUTPUT    KMSHIRED   PRTF.
*    システム日付／時間取得
     ACCEPT   WK-YMD    FROM   DATE.
     ACCEPT   WK-TIME   FROM   TIME.
     IF       WK-YMD(1:2)   >   91
              MOVE    19       TO      WK-YY
     ELSE
              MOVE    20       TO      WK-YY
     END-IF.
*    ワークエリアの初期化
     INITIALIZE         WK-DH-REC.
     MOVE     ZERO      TO      WK-DENCNT.
*    ＩＮＦＩＬＥ読込み
     PERFORM  FILE-RD-SEC.
*
 INIT-EXIT.
     EXIT.
******************************************************************
*    1.1      受信データ分割ファイル読込み
******************************************************************
 FILE-RD-SEC           SECTION.
*
     READ  INFILE  AT  END
           MOVE    "END"       TO        END-FLG
     END-READ.
*    ファイルヘッダの場合は，読み飛ばし
     IF    DH01  =  "A"
           GO                  TO        FILE-RD-SEC
     END-IF.
*
 FILE-RD-EXIT.
     EXIT.
******************************************************************
*    2.0      メイン処理
******************************************************************
 MAIN-SEC              SECTION.
*    ヘッダ情報レコード格納
     IF       DH01     =    "B"
              MOVE   SPACE   TO  WK-DH-REC
              INITIALIZE         WK-DH-REC
              MOVE   DH-REC  TO  WK-DH-REC
              GO             TO  MAIN010
     END-IF.
*    明細レコード編集
     IF       DH01     =    "D"
              PERFORM   HENSYU-SEC
     END-IF.
*    全体伝票枚数カウント
     IF       DD10  NOT =  WK-DD10
              IF    WK-DH04  =  5
                    ADD  1   TO  WK-DENCNT1
              ELSE
                    ADD  1   TO  WK-DENCNT
              END-IF
              MOVE  DD10     TO  WK-DD10
     END-IF.
*
 MAIN010.
     PERFORM  FILE-RD-SEC.
*
 MAIN-EXIT.
     EXIT.
******************************************************************
*    3.0      終了処理
******************************************************************
 END-SEC               SECTION.
*    件数リスト発行
     PERFORM  PRINT-SEC.
*    処理終了ファイルクローズ
     CLOSE    INFILE    KMSHIRED    PRTF.
*    伝票枚数更新の為伝票データファイル再オープン
     OPEN     I-O       KMSHIRED.
*    伝票データファイル読込み
     READ     KMSHIRED   AT  END
              IF      WK-DENCNT  NOT =  ZERO
                      DISPLAY "ﾃﾞﾝﾋｮｳﾏｲｽｳ ｺｳｼﾝ ｲｼﾞｮｳ" UPON CONS
              END-IF
              CLOSE    KMSHIRED
              GO       TO         END-EXIT
     END-READ.
*    伝票枚数転送
     ADD      1             TO    WK-DENCNT.
     MOVE     WK-DENCNT     TO    KMS31.
     REWRITE  KMS-REC.
*    伝票データファイルクローズ
     CLOSE    KMSHIRED.
*
 END-EXIT.
     EXIT.
******************************************************************
*    3.1      データ件数プリント
******************************************************************
 PRINT-SEC             SECTION.
*    システム日付取得
     ACCEPT  SYS-DATE6  FROM  DATE.
     MOVE     SYS-DATE6  TO    HEN-DATE.
     IF       HEN-YY   >  89
              COMPUTE  SYS-DATE8  =  19000000  +  SYS-DATE6
     ELSE
              COMPUTE  SYS-DATE8  =  20000000  +  SYS-DATE6
     END-IF.
     MOVE     SYS-DATE8  TO    HEN-SYS.
*    システム日付セット
     MOVE     SYS-YYYY   TO    HD01-YY.
     MOVE     SYS-MM     TO    HD01-MM.
     MOVE     SYS-DD     TO    HD01-DD.
*    頁カウンターセット
     MOVE     1          TO    HD01-PAGE.
*    ヘッダ行印字
     WRITE  PRT-REC  FROM  HD01  AFTER  2.
*    通常件数表示
     MOVE   NC"カーマ通常"     TO   MD01-USERNM.
     MOVE   WK-DENCNT          TO   MD01-KENSU.
     WRITE  PRT-REC  FROM  MD01  AFTER  5.
*    ２１件数表示
     MOVE   NC"カーマ２１"     TO   MD01-USERNM.
     MOVE   WK-DENCNT1         TO   MD01-KENSU.
     WRITE  PRT-REC  FROM  MD01  AFTER  2.
*
 PRINT-EXIT.
     EXIT.
******************************************************************
*    2.1      レコード編集処理
******************************************************************
 HENSYU-SEC            SECTION.
*伝票データファイルの初期化
     MOVE     SPACE         TO        KMS-REC.
     INITIALIZE                       KMS-REC.
 HENSYU010.
*    取引先コード
     MOVE     WK-DH03       TO        KMS01.
 HENSYU011.
*    伝票_
     MOVE     WK-DH06       TO        KMS02.
 HENSYU012.
*    行_
     MOVE     DD05          TO        KMS03.
 HENSYU013.
*    伝票区分
     MOVE     WK-DH09       TO        KMS04.
 HENSYU014.
*    部門
     MOVE     WK-DH08       TO        KMS05.
 HENSYU015.
*    発注企業名称
     MOVE     WK-DH13       TO        KMS07.
 HENSYU016.
*    発注店舗名称
     MOVE     WK-DH14       TO        KMS08.
 HENSYU017.
*    取引先名称
     MOVE     WK-DH12       TO        KMS09.
 HENSYU018.
*    発注日
     MOVE     WK-DH10       TO        KMS10.
 HENSYU019.
*    納品日
     MOVE     WK-DH11       TO        KMS11.
 HENSYU020.
*    ルート
     MOVE     WK-DH16       TO        WK-RUTO-S.
     MOVE     WK-RUTO-H     TO        KMS12.
 HENSYU021.
*    ＪＡＮコード
     MOVE     DD10          TO        KMS13.
 HENSYU022.
*    商品コード
     MOVE     DD06          TO        KMS14.
 HENSYU023.
*    商品名称１／２
     MOVE     DD08          TO        KMS15.
     MOVE     DD09          TO        KMS16.
 HENSYU024.
*    数値
     MOVE     DD11          TO        KMS17.
 HENSYU025.
*    原価単価
     MOVE     DD12          TO        KMS18.
 HENSYU026.
*    売価単価
     MOVE     DD13          TO        KMS19.
 HENSYU027.
*    原価金額
     COMPUTE  KMS20  =  DD11  *  DD12.
 HENSYU028.
*    売価金額
     COMPUTE  KMS21  =  DD11  *  DD13.
 HENSYU029.
*    商品区分
*****MOVE     DD14          TO        KMS22.
 HENSYU030.
*    店舗コード
     MOVE     WK-DH05       TO        KMS23.
 HENSYU031.
*    店舗名称
     MOVE     WK-DH14       TO        KMS24.
 HENSYU032.
*    伝票番号マーク
     MOVE     WK-DH07       TO        KMS25.
 HENSYU033.
*    法人コード
     MOVE     WK-DH04       TO        KMS26.
 HENSYU034.
*    特売コード
     MOVE     WK-DH20       TO        KMS27.
 HENSYU035.
*    備考
*****MOVE     WK-DH22       TO        KMS28.
 HENSYU036.
*    納品開始日
     MOVE     WK-DH17       TO        KMS29.
 HENSYU037.
*    納品終了日
     MOVE     WK-DH18       TO        KMS30.
 HENSYU038.
*    伝票枚数
     MOVE     ZERO          TO        KMS31.
 HENSYU039.
*    伝票名称
     MOVE     WK-DH15       TO        KMS32.
 HENSYU040.
*    館番号
     MOVE     WK-DH21       TO        KMS33.
 HENSYU051.
*    代行検品センター
     MOVE     WK-DH22       TO        KMS34.
 HENSYU052.
*    伝票データファイルＷＲＩＴＥ
     WRITE    KMS-REC.
*
 HENSYU-EXIT.
     EXIT.
********************<<  PROGRAM  END  >>**************************

```
