# VDA0732B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/VDA0732B.COB`

## ソースコード

```cobol
****************************************************************
*         SYSTEM･･･丸長商事オンラインシステム                  *
*        PG-NAME･･･物品受領書発行                              *
*          PG-ID･･･VDA0732B                                    *
*                                            DATE. 99.01.19    *
*                                              BY. T-TAKAHASHI *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            VDA0732B.
 AUTHOR.                T-TAKAHASHI.
 DATE-WRITTEN.          99/01/19.
*REMARKS.
*
******************************************************************
*                                                                *
 ENVIRONMENT            DIVISION.
*                                                                *
******************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM.
 OBJECT-COMPUTER.       FACOM.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS
         YA        IS   PICH-YA
         YB        IS   PICH-YB
         YB-21     IS   PICH-YA21.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*    検収データＦ
     SELECT   MKESHIRE           ASSIGN    TO   MKESHIRE
                                 ACCESS    MODE IS   SEQUENTIAL
                                 FILE STATUS    IS   MKE-STATUS.
*    画面ファイル
     SELECT   DSPFILE            ASSIGN    TO        GS-DSPF
                                 FORMAT              DSP-FMT
                                 GROUP               DSP-GRP
                                 PROCESSING          DSP-PRO
                                 FUNCTION            DSP-FNC
                                 STATUS              DSP-ST.
*    プリントファイル
     SELECT   PRINTF             ASSIGN    TO        LP-04.
*--------------------------------------------------------------*
 DATA                   DIVISION.
*--------------------------------------------------------------*
 FILE                   SECTION.
*****＜伝票データＦ＞*****
*    FILE = ｹﾝｼｭｳﾃﾞｰﾀﾌｱｲﾙ
 FD  MKESHIRE            BLOCK CONTAINS   3  RECORDS.
 01  MKE-REC.
     03  MKE01                    PIC       X(06).
     03  MKE02                    PIC       9(08).
     03  MKE03                    PIC       9(02).
     03  MKE04                    PIC       9(02).
     03  MKE05                    PIC       X(01).
     03  MKE06                    PIC       X(20).
     03  MKE07.
         05  MKE071               PIC       X(20).
         05  MKE072               PIC       X(20).
     03  MKE08                    PIC       9(06).
     03  MKE09                    PIC       9(06).
     03  MKE10                    PIC       X(03).
     03  MKE11.
         05  MKE111               PIC       X(20).
         05  MKE112               PIC       X(20).
     03  MKE12                    PIC       9(08).
     03  MKE13                    PIC       X(20).
     03  MKE14                    PIC       9(02).
     03  MKE15.
         05  MKE151               PIC       X(02).
         05  MKE152               PIC       X(06).
     03  MKE16.
         05  MKE161               PIC       X(20).
         05  MKE162               PIC       X(20).
     03  MKE17                    PIC       X(20).
     03  MKE18                    PIC       X(13).
     03  MKE19                    PIC       X(04).
     03  MKE20                    PIC       X(04).
     03  MKE21                    PIC       9(06)V9.
     03  MKE22                    PIC       9(07)V99.
     03  MKE23                    PIC       9(09).
     03  MKE24                    PIC       9(07).
     03  MKE25                    PIC       9(09).
     03  MKE26                    PIC       9(05).
     03  MKE27                    PIC       9(04).
     03  MKE28                    PIC       X(20).
     03  MKE29                    PIC       9(01).
     03  MKE30                    PIC       9(01).
     03  MKE31                    PIC       9(01).
     03  MKE32                    PIC       9(01).
     03  MKE33                    PIC       9(02).
     03  FILLER                   PIC       X(12).
*****＜画面　ファイル＞*****
 FD  DSPFILE            LABEL RECORD   IS   OMITTED.
*
     COPY    FVDA0732   OF   XMDLIB.
*
*****＜プリント　ファイル＞*****
 FD  PRINTF                       LINAGE    IS   33   LINES.
 01  PRT-REC                      PIC       X(200).
*
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
***  ｽﾃｰﾀｽ ｴﾘｱ
 01  STATUS-AREA.
     03  MKE-STATUS               PIC  X(02).
     03  PRT-STATUS               PIC  X(02).
***  ｶﾞﾒﾝ ｺﾝﾄﾛｰﾙ ｴﾘｱ
 01  DSP-CNTL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
     03  DSP-ST                   PIC  X(02).
***  ﾌﾗｸﾞ ｴﾘｱ
 01  FLG-AREA.
     03  END-FLG                  PIC  X(03)     VALUE   ZERO.
     03  ERR-FLG                  PIC  9(01)     VALUE   ZERO.
     03  SYORI-FLG                PIC  X(03)     VALUE   SPACE.
***  ｶｳﾝﾄ ｴﾘｱ
 01  CNT-AREA.
     03  L-CNT                    PIC  9(02)     VALUE   ZERO.
     03  CNT-AFTER                PIC  9(02)     VALUE   ZERO.
***  ｺﾞｳｹｲ ｴﾘｱ
 01  GOUKEI.
     03  G-GENKA                  PIC  9(09)     VALUE   ZERO.
     03  G-BAIKA                  PIC  9(09)     VALUE   ZERO.
***  ｹｲｻﾝ ｴﾘｱ
 01  KEISAN.
     03  W-GENKA                  PIC  9(09)V9   VALUE   ZERO.
     03  W-BAIKA                  PIC  9(09)     VALUE   ZERO.
***  日付エリア
 01  WK-SYS-DATE.
     03  WK-YY1                   PIC  9(02)     VALUE   ZERO.
     03  WK-YMD.
         05  WK-YY2               PIC  9(02)     VALUE   ZERO.
         05  WK-MM                PIC  9(02)     VALUE   ZERO.
         05  WK-DD                PIC  9(02)     VALUE   ZERO.
 01  WK-DATE                      PIC  9(08)     VALUE   ZERO.
 01  CNT-PAGE                     PIC  9(04)     VALUE   ZERO.
 01  WK-MKE-F02                   PIC  9(08)     VALUE   ZERO.
***  ﾜｰｸ  ｴﾘｱ
 01  WRK-AREA.
     03  WRK-MAI                  PIC  9(06)     VALUE   ZERO.
     03  WRK-R040                 PIC  9(01)     VALUE   ZERO.
     03  RD-SW                    PIC  9(01)     VALUE   ZERO.
     03  AAA                      PIC  X(01)     VALUE   SPACE.
     03  DENPYO.
         05  FILLER               PIC  X(22)     VALUE
             "ｼｲﾚ ﾃﾞﾝﾋﾟｮｳ ﾊｯｺｳ ﾏｲｽｳ ".
         05  CNT-DENPYO           PIC  9(09)     VALUE   ZERO.
*--- ﾃﾞﾝﾋﾟﾖｳ ｷ-
     03  WRK-DENNO.
         05  DENNO1               PIC  9(08)     VALUE   ZERO.
         05  SINSEINO             PIC  9(08)     VALUE   ZERO.
*    ﾒﾂｾ-ｼﾞ ｴﾘｱ
 01  MSG-AREA.
     03  MSG-FIELD.
         05  MSG01                PIC  N(30)     VALUE
         NC"物品受領書データが存在しません。".
         05  MSG02                PIC  N(30)     VALUE
         NC"正しい番号を入力してください。".
         05  MSG03                PIC  N(30)     VALUE
         NC"伝票発行中".
         05  MSG04                PIC  N(30)     VALUE
         NC"無効キーです".
     03  MSG-FIELD-R     REDEFINES    MSG-FIELD.
         05  MSG-TBL     OCCURS     4      PIC  N(30).
*
 01  FILE-ERR010                  PIC  N(10)     VALUE
         NC"プリンター　異常！！".
 01  FILE-ERR020                  PIC  N(14)     VALUE
         NC"印刷を続けますか？　Ｙ／Ｎ".
 01  FILE-ERR030                  PIC  N(11)     VALUE
         NC"伝票データＦ　異常！！".
 01  FILE-ERR040                  PIC  N(11)     VALUE
         NC"画面ファイル　異常！！".
***  ﾌﾟﾘﾝﾄ ｴﾘｱ
*    ﾐﾀﾞｼ
 01  HEAD-01.
     03  FILLER                   PIC  X(40)     VALUE   SPACE.
     03  FILLER                   PIC  N(16)     VALUE
         NC"＊＊　丸長商事　物品受領書　＊＊"
         CHARACTER  TYPE  IS  PICH-YA21.
     03  FILLER                   PIC  X(15)     VALUE   SPACE.
     03  FILLER                   PIC  X(05)     VALUE   "DATE:".
     03  SYS-DT-YY                PIC  9(04).
     03  FILLER                   PIC  X(01)     VALUE   "/".
     03  SYS-DT-MM                PIC  9(02).
     03  FILLER                   PIC  X(01)     VALUE   "/".
     03  SYS-DT-DD                PIC  9(02).
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  FILLER                   PIC  X(05)     VALUE   "PAGE:".
     03  PG-CNT                   PIC  9(04).
*
 01  HEAD-02                      CHARACTER  TYPE  IS  PICH-YA.
     03  FILLER                   PIC  X(05)     VALUE   SPACE.
     03  FILLER                   PIC  N(07)     VALUE
         NC"社名：丸長商事".
*
 01  HEAD-03                      CHARACTER  TYPE  IS  PICH-YA.
     03  FILLER                   PIC  X(05)     VALUE   SPACE.
     03  FILLER                   PIC  N(03)     VALUE
         NC"店名：".
     03  HD03-TENCD               PIC  9(04).
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  HD03-TENMEI              PIC  X(12).
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  FILLER                   PIC  N(03)     VALUE
         NC"分類：".
     03  HD03-BUNRUI              PIC  9(02).
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  FILLER                   PIC  N(05)     VALUE
         NC"発注区分：".
     03  HD03-HATYU               PIC  9(01).
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  FILLER                   PIC  N(05)     VALUE
         NC"伝票区分：".
     03  HD03-DENKU               PIC  9(01).
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  HD03-DENKU-MEI           PIC  N(05).
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  FILLER                   PIC  N(05)     VALUE
         NC"伝票番号：".
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  HD03-DENNO               PIC  Z9999999.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  FILLER                   PIC  N(04)     VALUE
         NC"発注日：".
     03  HD03-H-YY                PIC  9(02).
     03  FILLER                   PIC  X(01)     VALUE   "/".
     03  HD03-H-MM                PIC  9(02).
     03  FILLER                   PIC  X(01)     VALUE   "/".
     03  HD03-H-DD                PIC  9(02).
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  FILLER                   PIC  N(04)     VALUE
         NC"検収日：".
     03  HD03-K-YY                PIC  9(02).
     03  FILLER                   PIC  X(01)     VALUE   "/".
     03  HD03-K-MM                PIC  9(02).
     03  FILLER                   PIC  X(01)     VALUE   "/".
     03  HD03-K-DD                PIC  9(02).
*
 01  HEAD-031                     CHARACTER  TYPE  IS  PICH-YA.
     03  FILLER                   PIC  X(05)     VALUE   SPACE.
     03  FILLER                   PIC  N(04)     VALUE
         NC"取引先：".
     03  HD03-TORICD              PIC  9(06).
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  HD03-TORIMEI             PIC  X(40).
*
 01  HEAD-04                      CHARACTER  TYPE  IS  PICH-YA.
     03  FILLER                   PIC  X(05)     VALUE   SPACE.
     03  FILLER                   PIC  N(01)     VALUE
         NC"行".
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  FILLER                   PIC  N(04)     VALUE
         NC"商品名称".
     03  FILLER                   PIC  X(14)     VALUE   SPACE.
     03  FILLER                   PIC  N(05)     VALUE
         NC"商品コード".
     03  FILLER                   PIC  X(07)     VALUE   SPACE.
     03  FILLER                   PIC  N(02)     VALUE
         NC"数量".
     03  FILLER                   PIC  X(05)     VALUE   SPACE.
     03  FILLER                   PIC  N(04)     VALUE
         NC"原価単価".
     03  FILLER                   PIC  X(05)     VALUE   SPACE.
     03  FILLER                   PIC  N(04)     VALUE
         NC"原価金額".
     03  FILLER                   PIC  X(05)     VALUE   SPACE.
     03  FILLER                   PIC  N(04)     VALUE
         NC"売価単価".
     03  FILLER                   PIC  X(07)     VALUE   SPACE.
     03  FILLER                   PIC  N(04)     VALUE
         NC"売価金額".
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  FILLER                   PIC  N(02)     VALUE
         NC"理由".
     03  FILLER                   PIC  X(08)     VALUE   SPACE.
*
 01  BODY-01.
     03  FILLER                   PIC  X(05)     VALUE   SPACE.
     03  BD01-GYO                 PIC  Z9.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  BD01-SYOUHIN             PIC  X(20).
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  BD01-JANCD               PIC  X(13).
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  BD01-SURYO               PIC  Z,ZZ9.
     03  FILLER                   PIC  X(03)     VALUE   SPACE.
     03  BD01-GENKA-TANKA         PIC  ZZZ,ZZ9.99.
     03  FILLER                   PIC  X(03)     VALUE   SPACE.
     03  BD01-GENKA-KINGAKU       PIC  ZZZ,ZZZ,ZZ9.
     03  FILLER                   PIC  X(03)     VALUE   SPACE.
     03  BD01-BAIKA-TANKA         PIC  ZZZ,ZZ9.99.
     03  FILLER                   PIC  X(03)     VALUE   SPACE.
     03  BD01-BAIKA-KINGAKU       PIC  ZZZ,ZZZ,ZZ9.
     03  FILLER                   PIC  X(03)     VALUE   SPACE.
     03  BD01-RIYU                PIC  9(02).
*
 01  BODY-02.
     03  FILLER                   PIC  X(09)     VALUE   SPACE.
     03  BD02-KIKAKU              PIC  X(20).
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  BD01-SYOCD               PIC  9(07).
*    ｺﾞｳｹｲ
 01  TAIL-01.
     03  FILLER                   PIC  X(67)     VALUE   SPACE.
     03  TL01                     PIC  ZZZ,ZZZ,ZZ9.
     03  FILLER                   PIC  X(16)     VALUE   SPACE.
     03  TL02                     PIC  ZZZ,ZZZ,ZZ9.
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
******************************************************************
*                       SHORI                         0.0.0      *
******************************************************************
 DECLARATIVES.
*--- << ﾌﾟﾘﾝﾀ- ｴﾗ- >> ---*
 000-PRINTF-ERR         SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      PRINTF.
     DISPLAY  FILE-ERR010      UPON    CONS.
     ACCEPT   AAA              FROM    CONS.
     STOP     RUN.
*--- << ｼｲﾚ ﾃﾞ-ﾀ ｴﾗ- >> ---*
 000-MAST-ERR           SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      MKESHIRE.
     DISPLAY  FILE-ERR030      UPON    CONS.
     ACCEPT   AAA              FROM    CONS.
     DISPLAY  MKE-STATUS       UPON    CONS.
     ACCEPT   AAA              FROM    CONS.
     STOP     RUN.
*--- << ｶﾞﾒﾝ    ｴﾗ- >> ---*
 000-DSPFILE-ERR        SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      DSPFILE.
     DISPLAY  FILE-ERR040      UPON    CONS.
     ACCEPT   AAA              FROM    CONS.
     DISPLAY  DSP-ST           UPON    CONS.
     ACCEPT   AAA              FROM    CONS.
     STOP     RUN.
 END DECLARATIVES.
******************************************************************
*            M  A  I  N          M  O  D  U  L  E                *
******************************************************************
 CONTROL-START          SECTION.
*
     PERFORM            INIT-SEC.
     PERFORM            MAIN-SEC
                        UNTIL     END-FLG  =  "END".
     PERFORM            END-SEC.
*
 CONTROL-END.
     STOP     RUN.
****************************************************************
*             初期処理                              1.0        *
****************************************************************
 INIT-SEC               SECTION.
*    ファイルのＯＰＥＮ
     OPEN     INPUT     MKESHIRE.
     OPEN     I-O       DSPFILE.
*    ワーク初期化
     MOVE     ZERO               TO   DENNO1.
     MOVE     ZERO               TO   CNT-DENPYO.
     MOVE     SPACE              TO   END-FLG.
     MOVE     SPACE              TO   FVDA0732.
*    システム日付取得
     ACCEPT   WK-YMD  FROM  DATE.
     IF       WK-YY2  >  91
              MOVE    19         TO   WK-YY1
     ELSE
              MOVE    20         TO   WK-YY1
     END-IF.
*    伝票枚数カウント
 INIT001.
     READ  MKESHIRE  AT  END
           GO        TO       INIT002
     END-READ.
*    伝票_ブレイクチェック
     IF    MKE02    NOT =  WK-MKE-F02
           MOVE     MKE02     TO    WK-MKE-F02
           ADD      1         TO    WRK-MAI
           GO                 TO    INIT001
     ELSE
           GO                 TO    INIT001
     END-IF.
*    伝票_ゼロ件処理（エラーメッセージ出力）
 INIT002.
     IF    WRK-MAI  <=  0
           MOVE     MSG01     TO     MD05
     END-IF.
*    ファイルＯＰＥＮ／ＣＬＯＳＥ
     CLOSE          MKESHIRE.
     OPEN   INPUT   MKESHIRE.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                              2.0      *
****************************************************************
 MAIN-SEC               SECTION.
*    画面の初期化
     MOVE          SPACE     TO   SYORI-FLG.
     MOVE          SPACE     TO   FVDA0732.
*    画面表示
     PERFORM       DSP-WRITE-SEC.
*    キー入力
     MOVE         "MD04R"    TO   DSP-GRP.
     PERFORM       DSP-READ-SEC.
*    アテンション判定
     EVALUATE      DSP-FNC
         WHEN     "F005"
                   MOVE     "END"      TO   END-FLG
                   GO                  TO   MAIN-EXIT
         WHEN     "E000"
                   CONTINUE
         WHEN      OTHER
                   MOVE      4         TO   ERR-FLG
     END-EVALUATE.
*処理対象未入力時処理
     IF   MD04  NOT  NUMERIC
          MOVE     2    TO   ERR-FLG
          PERFORM       ERR-MSG-SEC
          GO       TO   MAIN-SEC
     ELSE
          MOVE  MD04    TO   WRK-R040
     END-IF.
*
     PERFORM       ERR-MSG-SEC.
     PERFORM       DSP-WRITE-SEC.
*    プリントファイルのＯＰＥＮ
     OPEN     OUTPUT    PRINTF.
*    処理判定
     EVALUATE      MD04
*        テストプリント
         WHEN      1
                   MOVE      2         TO   ERR-FLG
*        全プリント
         WHEN      2
                   MOVE      ZERO      TO   MD02
                   MOVE      ALL "9"   TO   MD03
                   MOVE      3         TO   ERR-FLG
                   PERFORM   ERR-MSG-SEC
                   PERFORM   DSP-WRITE-SEC
                   PERFORM   FL-READ-SEC
                   PERFORM   DENP-WT-SEC
                             UNTIL     SYORI-FLG =   "END"
*        範囲指定
         WHEN      3
                   MOVE      2         TO   ERR-FLG
         WHEN      OTHER
                   MOVE      2         TO   ERR-FLG
     END-EVALUATE.
*    プロリントファイルＣＬＯＳＥ
     CLOSE         PRINTF    MKESHIRE.
*    次対象の入力為，ＲＥＡＤスイッチＯＦＦ
     MOVE          ZERO      TO       RD-SW.
*    伝票データＦＯＰＥＮ
     OPEN          INPUT     MKESHIRE.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             エラーメッセージセット                2.1        *
****************************************************************
 ERR-MSG-SEC            SECTION.
*
     IF       ERR-FLG   =   ZERO
              MOVE   SPACE                 TO   MD05
     ELSE
              MOVE   MSG-TBL ( ERR-FLG )   TO   MD05
              MOVE   ZERO                  TO   ERR-FLG
     END-IF.
*
 ERR-MSG-EXIT.
     EXIT.
****************************************************************
*             画面表示処理                           2.2       *
****************************************************************
 DSP-WRITE-SEC          SECTION.
*    伝票総枚数セット
     MOVE     WRK-MAI   TO        MD01.
     MOVE     SPACE     TO        DSP-CNTL.
     MOVE    "SCREEN"   TO        DSP-GRP.
     MOVE    "FVDA0732" TO        DSP-FMT.
*    画面表示
     WRITE    FVDA0732.
 DSP-WRITE-EXIT.
     EXIT.
****************************************************************
*             画面ＲＥＡＤ処理                      2.3        *
****************************************************************
 DSP-READ-SEC           SECTION.
*
     MOVE    "NE"       TO        DSP-PRO.
     READ     DSPFILE.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*             範囲指定処理                            2.4      *
****************************************************************
 KEY-IN-SEC             SECTION.
*
 KEY-IN-01.
     PERFORM       ERR-MSG-SEC.
     PERFORM       DSP-WRITE-SEC.
*    範囲指定
     MOVE         "KEY01"    TO   DSP-GRP.
     MOVE          ZERO      TO   MD02      MD03.
*    画面読込み
     PERFORM       DSP-READ-SEC.
*    アテンション判定
     EVALUATE      DSP-FNC
*        終了
         WHEN     "F005"
                   MOVE     "END"      TO   END-FLG
                   GO                  TO   KEY-IN-EXIT
*        実行
         WHEN     "E000"
                   CONTINUE
         WHEN      OTHER
                   MOVE      4         TO   ERR-FLG
                   GO                  TO   KEY-IN-EXIT
     END-EVALUATE.
*    範囲指定大小チェック
     IF            MD02   >  MD03
                   MOVE      2         TO   ERR-FLG
                   GO                  TO   KEY-IN-01
     END-IF.
*    未入力時（全データ対象）
     IF           (MD02   =  ZERO)     AND
                  (MD03   =  ZERO)
                   MOVE      ZERO      TO   MD02
                   MOVE      ALL "9"   TO   MD03
     END-IF.
*
 KEY-IN-EXIT.
     EXIT.
****************************************************************
*             伝票出力処理                          2.5        *
****************************************************************
 DENP-WT-SEC            SECTION.
*    伝票_のブレイク判定
     IF       MKE02     NOT =     DENNO1
              PERFORM   TAIL-WT-SEC
              PERFORM   HEAD-WT-SEC
              MOVE      MKE02     TO   DENNO1
              MOVE      MKE12     TO   SINSEINO
     END-IF.
*    行カウンタの判定
     IF       L-CNT        >=     23
              PERFORM   TAIL-WT-SEC
              PERFORM   HEAD-WT-SEC
     END-IF.
*    項目初期化
     INITIALIZE                  BODY-01
                                 BODY-02.
*    明細項目セット
*    行_
     MOVE     MKE03              TO   BD01-GYO.
*    品名
     MOVE     MKE161             TO   BD01-SYOUHIN.
*    品名
     MOVE     MKE162             TO   BD02-KIKAKU.
*    ＪＡＮコード
     MOVE     MKE18              TO   BD01-JANCD.
*    ＪＡＮコード
     MOVE     MKE15              TO   BD01-SYOCD.
*    数量
     MOVE     MKE21(1:6)         TO   BD01-SURYO.
*    原価単価
     MOVE     MKE22              TO   BD01-GENKA-TANKA.
*    原価金額
     COMPUTE  W-GENKA   =    MKE21    *  MKE22.
     MOVE     W-GENKA            TO   BD01-GENKA-KINGAKU.
     COMPUTE  G-GENKA   =    G-GENKA  +  W-GENKA.
*    売価単価
     MOVE     MKE24              TO   BD01-BAIKA-TANKA.
*    売価金額
     COMPUTE  W-BAIKA   =    MKE21    *  MKE24.
     MOVE     W-BAIKA            TO   BD01-BAIKA-KINGAKU.
     COMPUTE  G-BAIKA   =    G-BAIKA  +  W-BAIKA.
*    理由
     MOVE     MKE33              TO   BD01-RIYU.
*
     WRITE    PRT-REC        FROM     BODY-01   AFTER     1.
     WRITE    PRT-REC        FROM     BODY-02   AFTER     1.
*
     ADD      2                  TO   L-CNT.
*
     PERFORM  FL-READ-SEC.
*
     IF       SYORI-FLG   =        "END"
              PERFORM   TAIL-WT-SEC
     END-IF.
 DENP-WT-EXIT.
     EXIT.
****************************************************************
*             ファイルＲＥＡＤ処理                  2.5.1      *
****************************************************************
 FL-READ-SEC            SECTION.
*    伝票データ読込み
 READ-000.
     READ     MKESHIRE  AT        END
              MOVE     "END"      TO   SYORI-FLG
              MOVE      1         TO   ERR-FLG
              PERFORM   ERR-MSG-SEC
              GO                  TO   FL-READ-EXIT
     END-READ.
*    全プリント時判定
     IF       MD04    =    2
              GO                  TO   READ-010
     END-IF.
*    伝票番号範囲指定大小チェック
     IF       MKE02   <    MD02
              GO                  TO   READ-000
     END-IF.
     IF       MKE02   >    MD03
              GO                  TO   READ-000
     END-IF.
 READ-010.
*    １件目時処理
     IF       RD-SW     =    0
              MOVE      MKE02     TO   DENNO1
              PERFORM   HEAD-WT-SEC
              MOVE      1         TO   RD-SW
     END-IF.
*
 FL-READ-EXIT.
     EXIT.
****************************************************************
*             ヘッド部出力処理                      2.5.2      *
****************************************************************
 HEAD-WT-SEC            SECTION.
*    ヘッド部項目初期化
     INITIALIZE                  HEAD-01
                                 HEAD-02
                                 HEAD-03
                                 HEAD-04.
*--- 項目セット
*    日付セット
     MOVE     WK-SYS-DATE      TO      WK-DATE.
     MOVE     WK-DATE(1:4)     TO      SYS-DT-YY.
     MOVE     WK-DATE(5:2)     TO      SYS-DT-MM.
     MOVE     WK-DATE(7:2)     TO      SYS-DT-DD.
*    頁セット
     ADD      1                TO      CNT-PAGE.
     MOVE     CNT-PAGE         TO      PG-CNT.
*    店舗情報
     MOVE     MKE26            TO      HD03-TENCD.
     MOVE     MKE28            TO      HD03-TENMEI.
*    分類
     MOVE     MKE04            TO      HD03-BUNRUI.
*    発注区分
     MOVE     MKE32            TO      HD03-HATYU.
*    伝票区分
     MOVE     MKE05            TO      HD03-DENKU.
*    伝票番号
     MOVE     MKE02            TO      HD03-DENNO.
*    取引先
     MOVE     MKE01            TO      HD03-TORICD.
     MOVE     MKE06            TO      HD03-TORIMEI.
*    発注日
     MOVE     MKE08(1:2)       TO      HD03-H-YY.
     MOVE     MKE08(3:2)       TO      HD03-H-MM.
     MOVE     MKE08(5:2)       TO      HD03-H-DD.
*    検収日
     MOVE     MKE09(1:2)       TO      HD03-K-YY.
     MOVE     MKE09(3:2)       TO      HD03-K-MM.
     MOVE     MKE09(5:2)       TO      HD03-K-DD.
*    伝票区分
     EVALUATE  MKE05
         WHEN  "1"
               MOVE  NC"ＥＯＳ仕入"  TO  HD03-DENKU-MEI
         WHEN  "2"
               MOVE  NC"手書き仕入"  TO  HD03-DENKU-MEI
         WHEN  "3"
               MOVE  NC"返品　　　"  TO  HD03-DENKU-MEI
         WHEN  OTHER
               MOVE  NC"＊＊＊＊＊"  TO  HD03-DENKU-MEI
     END-EVALUATE.
*
     WRITE    PRT-REC          FROM   HEAD-01    AFTER    5.
*****WRITE    PRT-REC          FROM   HEAD-02    AFTER    1.
     WRITE    PRT-REC          FROM   HEAD-03    AFTER    2.
     WRITE    PRT-REC          FROM   HEAD-031   AFTER    1.
     WRITE    PRT-REC          FROM   HEAD-04    AFTER    2.
*    印字位置送り
     MOVE     SPACE            TO     PRT-REC.
     WRITE    PRT-REC          AFTER  1.
*    行カウンタ
     MOVE     11               TO     L-CNT.
*
 HEAD-WT-EXIT.
     EXIT.
****************************************************************
*             テール部出力処理                      2.5.3      *
****************************************************************
 TAIL-WT-SEC            SECTION.
*    テイル部初期化
     INITIALIZE                  TAIL-01.
*    原価金額合計
     MOVE   G-GENKA              TO   TL01.
*    売価金額合計
     MOVE   G-BAIKA              TO   TL02.
*    テイル印字制御
     COMPUTE   CNT-AFTER   =     25  -  L-CNT.
*    テイル行の印字
     WRITE    PRT-REC          FROM   TAIL-01   AFTER   CNT-AFTER.
*    次頁へ改頁
     MOVE     SPACE            TO     PRT-REC.
     WRITE    PRT-REC          AFTER  PAGE.
*    ワーク初期化
     MOVE     ZERO             TO     G-GENKA
                                      G-BAIKA
*    伝票枚数
     ADD      1                TO     CNT-DENPYO.
*
 TAIL-WT-EXIT.
     EXIT.
****************************************************************
*             終了処理                              3.0        *
****************************************************************
 END-SEC                SECTION.
*    ファイルのＣＬＯＳＥ
     CLOSE    MKESHIRE  DSPFILE.
*
 END-EXIT.
     EXIT.

```
