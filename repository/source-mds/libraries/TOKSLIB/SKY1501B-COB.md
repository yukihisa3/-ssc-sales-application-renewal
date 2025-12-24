# SKY1501B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SKY1501B.COB`

## ソースコード

```cobol
****************************************************************
*         SYSTEM･･･_サカタのタネ　共通システム                *
*        PG-NAME･･･発注集計表データ抽出                        *
*          PG-ID･･･SKY1501B                                    *
*                                            DATE. 99.06.30    *
*                                              BY. HAGIWARA    *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SKY1501B.
 AUTHOR.                HAGIWARA.
 DATE-WRITTEN.          99/06/30.
*REKYOKS.
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
            YA     IS   YA.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*    伝票データ
     SELECT   HDENJNL            ASSIGN    TO    DA-01-VI-HDENJNL
                                 ORGANIZATION   IS   INDEXED
                                 ACCESS    MODE IS   SEQUENTIAL
                                 RECORD    KEY  IS   DEN-F01
                                                     DEN-F02
                                                     DEN-F04
                                                     DEN-F051
                                                     DEN-F03
                                 FILE STATUS    IS   DEN-ST.
*    集計表データ　Ｆ
     SELECT   UHACHUD   ASSIGN    TO        UHACHUD
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   UHAC-ST.
*
*    画面ファイル
     SELECT   DSPFILE            ASSIGN    TO        GS-DSPF
                                 FORMAT              DSP-FMT
                                 GROUP               DSP-GRP
                                 PROCESSING          DSP-PRO
                                 FUNCTION            DSP-FNC
                                 STATUS              DSP-ST.
*----<< 取引先マスタ >>-*
     SELECT   HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TOK-F01
                        FILE      STATUS    TOK-ST.
*--------------------------------------------------------------*
 DATA                   DIVISION.
*--------------------------------------------------------------*
 FILE                   SECTION.
*****＜伝票データ＞*****
 FD  HDENJNL
     BLOCK       CONTAINS  16        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        SHTDENF   OF        XFDLIB
     JOINING     DEN       AS        PREFIX.
*****＜集計表データＦ＞*****
 FD  UHACHUD            BLOCK CONTAINS   5  RECORDS.
 01  UHAC-REC.
     03  UHAC01                   PIC       X(02).
     03  UHAC02.
         05  UHAC02A              PIC       9(04).
         05  UHAC02B              PIC       9(05).
     03  UHAC03                   PIC       X(04).
     03  UHAC04                   PIC       9(01).
     03  UHAC05                   PIC       9(06).
     03  UHAC06                   PIC       9(06).
     03  UHAC07                   PIC       X(08).
     03  UHAC08                   PIC       X(13).
     03  UHAC09                   PIC       9(03)V9.
     03  UHAC10                   PIC       9(04).
     03  UHAC11                   PIC       9(05)V9.
     03  UHAC12                   PIC       9(07)V9(02).
     03  UHAC13                   PIC       9(07).
     03  UHAC14                   PIC       9(06).
     03  UHAC15                   PIC       X(25).
     03  UHAC16                   PIC       X(20).
     03  UHAC17                   PIC       X(13).
     03  UHAC18                   PIC       X(20).
     03  UHAC19                   PIC       9(03).
     03  UHAC20                   PIC       X(02).
     03  UHACFIL                  PIC       X(32).
*

*****＜画面　ファイル＞*****
 FD  DSPFILE            LABEL RECORD   IS   OMITTED.
*
     COPY    FKY15011   OF   XMDLIB.
*
*----<< 取引先マスタ >>-*
 FD  HTOKMS             BLOCK     CONTAINS   8   RECORDS
                        LABEL     RECORD     IS  STANDARD.
     COPY     HTOKMS    OF   XFDLIB    JOINING   TOK  AS   PREFIX.
*
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
***  ｽﾃｰﾀｽ ｴﾘｱ
 01  STATUS-AREA.
     03  DEN-ST                   PIC  X(02).
     03  UHAC-ST                  PIC  X(02).
*    03  DSP-ST                   PIC  X(02).
     03  TOK-ST                   PIC  X(02).
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
***  ﾜｰｸ  ｴﾘｱ
 01  WRK-AREA.
     03  AAA                      PIC  X(01)     VALUE   SPACE.
     03  WK-TOKKN                 PIC  X(15)     VALUE   SPACE.
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                 PIC  9(08).
*画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
*    ﾒﾂｾ-ｼﾞ ｴﾘｱ
 01  MSG-AREA.
     03  MSG-FIELD.
         05  MSG01                PIC  N(30)     VALUE
         NC"　　　　　　　　　　　　　　　".
         05  MSG02                PIC  N(30)     VALUE
         NC"正しい番号を入力してください。".
         05  MSG03                PIC  N(30)     VALUE
         NC"伝票発行中。".
         05  MSG04                PIC  N(30)     VALUE
         NC"無効キーです。".
         05  MSG05                PIC  N(30)     VALUE
         NC"取引先コードを入力してください。".
         05  MSG06                PIC  N(30)     VALUE
         NC"取引先Ｍに存在しません。取引先Ｍを登録して下さい。".
         05  MSG07                PIC  N(30)     VALUE
         NC"指定された取引先の伝票データが存在しません。".
         05  MSG08                PIC  N(30)     VALUE
         NC"発注集計表データ作成中！！".
     03  MSG-FIELD-R     REDEFINES    MSG-FIELD.
         05  MSG-TBL     OCCURS     8      PIC  N(30).
*
 01  FILE-ERR030                  PIC  N(11)     VALUE
         NC"伝票データＦ　異常！！".
 01  FILE-ERR040                  PIC  N(11)     VALUE
         NC"画面ファイル　異常！！".
 01  FILE-ERR050                  PIC  N(11)     VALUE
         NC"得意先マスタ　異常！！".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
******************************************************************
*                       SHORI                         0.0.0      *
******************************************************************
 DECLARATIVES.
*--- << ﾃﾞﾝﾋﾟｮｳ ﾃﾞ-ﾀ ｴﾗ- >> ---*
 000-MAST-ERR           SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      HDENJNL.
     DISPLAY  FILE-ERR030      UPON    CONS.
     ACCEPT   AAA              FROM    CONS.
     DISPLAY  DEN-ST           UPON    CONS.
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
     OPEN     INPUT     HDENJNL.
     OPEN     OUTPUT    UHACHUD.
     OPEN     INPUT     HTOKMS.
     OPEN     I-O       DSPFILE.
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
*画面表示日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*    ワーク初期化
     MOVE     SPACE              TO   END-FLG.
     MOVE     SPACE              TO   FKY15011.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                              2.0      *
****************************************************************
 MAIN-SEC               SECTION.
*****画面の初期化*****
     MOVE          SPACE     TO   FKY15011.
 MAIN010.
*****SYORI-FLG初期化*****
     MOVE          SPACE     TO   SYORI-FLG.
*****画面表示*****
     PERFORM       DSP-WRITE-SEC.
*****取引先入力*****
     MOVE         "HANI"    TO   DSP-GRP.
     PERFORM       DSP-READ-SEC.
*****アテンション判定*****
     EVALUATE      DSP-FNC
         WHEN     "F004"
                   GO                  TO   MAIN-EXIT
         WHEN     "F005"
                   MOVE     4010       TO   PROGRAM-STATUS
                   MOVE     "END"      TO   END-FLG
                   GO                  TO   MAIN-EXIT
         WHEN     "E000"
                   CONTINUE
         WHEN      OTHER
                   MOVE      4         TO   ERR-FLG
     END-EVALUATE.
*****取引先入力チェック*****
     IF   TOKCD    NOT   NUMERIC
     OR   TOKCD    =     ZERO
          MOVE     5    TO   ERR-FLG
          PERFORM       ERR-MSG-SEC
          GO       TO   MAIN010
     ELSE
*****取引先マスタ検索*****
          MOVE     TOKCD     TO   TOK-F01
          READ     HTOKMS    INVALID
                   MOVE     6    TO   ERR-FLG
                   PERFORM       ERR-MSG-SEC
                   GO       TO   MAIN010
                   NOT  INVALID
                   MOVE  TOK-F03 TO   TOKNM
                   MOVE  TOK-F04 TO   WK-TOKKN
          END-READ
     END-IF.
     CLOSE    HDENJNL.
     OPEN     INPUT  HDENJNL.
*****未入力時（全データ対象）*****
     IF            MD02      NOT       NUMERIC
                   MOVE      ZERO      TO   MD02
     END-IF.
     IF            MD03      NOT       NUMERIC
                   MOVE      ALL "9"   TO   MD03
     END-IF.
*****範囲指定大小チェック*****
     IF            MD02   >  MD03
                   MOVE      2         TO   ERR-FLG
                   PERFORM             ERR-MSG-SEC
                   GO                  TO   MAIN010
     END-IF.
*****キー項目セット*****
     MOVE          TOKCD     TO        DEN-F01.
     MOVE          ZERO      TO        DEN-F02.
     MOVE          ZERO      TO        DEN-F04
                                       DEN-F051
                                       DEN-F03.
*****売上ファイルスタート*****
     START  HDENJNL  KEY  IS >=  DEN-F01 DEN-F02 DEN-F04
                                 DEN-F051 DEN-F03
          INVALID  KEY
              MOVE    7         TO        ERR-FLG
              PERFORM           ERR-MSG-SEC
              GO      TO        MAIN010
     END-START.
*    伝票ファイル読込み
     PERFORM FL-READ-SEC.
*
     IF       SYORI-FLG   =    "END"
              CLOSE      HDENJNL
              OPEN       INPUT  HDENJNL
              MOVE    7  TO     ERR-FLG
              PERFORM    ERR-MSG-SEC
              GO         TO   MAIN010
     END-IF.
*
*****DISPLAY "TOKCD1 = " TOKCD UPON CONS.
     MOVE    8         TO        ERR-FLG.
     PERFORM           ERR-MSG-SEC.
     MOVE    ZERO      TO        ERR-FLG.
     PERFORM           DSP-WRITE-SEC.
*******************************************
*    発注集計表データ書込
*******************************************
 MAIN020.
*    集計表データＦの初期化
     MOVE     SPACE       TO        UHAC-REC.
     INITIALIZE                     UHAC-REC.
*    データ区分
     MOVE     SPACE       TO        UHAC01.
*    社コード
*    MOVE     SPACE       TO        UHAC02A.
*    店舗コード
     MOVE     DEN-F07     TO        UHAC02B.
*    部門（Ｈ）
     MOVE     DEN-F12     TO        UHAC03.
*    商品区分
     MOVE     DEN-F131    TO        UHAC04.
*    発注日
     MOVE     DEN-F111    TO        UHAC05.
*    納品日
     MOVE     DEN-F112    TO        UHAC06.
*    取引先コード
     MOVE     DEN-F01     TO        UHAC07.
*    商品コード
     MOVE     DEN-F1411   TO        UHAC08.
*    入数
     MOVE     ZERO        TO        UHAC09.
*    ケース数
     MOVE     ZERO        TO        UHAC10.
*    数量
     MOVE     DEN-F15     TO        UHAC11.
*    原価単価
     MOVE     DEN-F172    TO        UHAC12.
*    売価単価
     MOVE     DEN-F173    TO        UHAC13.
*    外注_
     MOVE     ZERO        TO        UHAC14.
*    商品名
     MOVE     DEN-F1421   TO        UHAC15.
*    規格
     MOVE     DEN-F1422   TO        UHAC16.
*    取引先商品コード
     MOVE     DEN-F25     TO        UHAC17.
*    発注者名
     MOVE     WK-TOKKN    TO        UHAC18.
*    店ソートコード
     MOVE     DEN-F07(3:3) TO       UHAC19.
*    ルート
     MOVE     SPACE       TO        UHAC20.
*    集計表データファイル
     WRITE    UHAC-REC.
*    伝票データ読込み
     PERFORM  FL-READ-SEC.
*
*    伝票データ終了判断　　
     IF       SYORI-FLG   =   "END"
              MOVE        "END"     TO    END-FLG
              GO          TO  MAIN-EXIT
     ELSE     GO          TO  MAIN020
     END-IF.
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
     MOVE     SPACE     TO        DSP-CNTL.
     MOVE    "SCREEN"   TO        DSP-GRP.
     MOVE    "FKY15011" TO        DSP-FMT.
     MOVE     HEN-DATE  TO        SDATE.
     MOVE     HEN-TIME  TO        STIME.
*    画面表示
     WRITE    FKY15011.
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
*             ファイルＲＥＡＤ処理                  2.5.1      *
****************************************************************
 FL-READ-SEC            SECTION.
*    伝票データ読込み
 READ-000.
     READ     HDENJNL   AT        END
              MOVE     "END"      TO   SYORI-FLG
              MOVE      1         TO   ERR-FLG
              PERFORM   ERR-MSG-SEC
              GO                  TO   FL-READ-EXIT
     END-READ.
*画面入力された取引先コード以外の場合
     IF       DEN-F01   >   TOKCD
              MOVE     "END"      TO   SYORI-FLG
              MOVE      1         TO   ERR-FLG
              PERFORM   ERR-MSG-SEC
              GO                  TO   FL-READ-EXIT
     END-IF.
*行番号チェック（２０以上は対象外）
     IF       DEN-F03 >    20
              GO                  TO   READ-000
     END-IF.
*売上作成ＦＬＧ
     IF       DEN-F277  =  9
              GO                  TO   READ-000
     END-IF.
*オンライン
     IF       DEN-F274  NOT =  0
              GO                  TO   READ-000
     END-IF.
*相殺区分
     IF       DEN-F04   NOT =  0
              GO                  TO   READ-000
     END-IF.
*伝区
     IF       DEN-F051  NOT =  40
              GO                  TO   READ-000
     END-IF.
*伝発
     IF       DEN-F134  NOT =  0
              GO                  TO   READ-000
     END-IF.
*    伝票番号範囲指定大小チェック
     IF       DEN-F02 <    MD02
              GO                  TO   READ-000
     END-IF.
     IF       DEN-F02 >    MD03
              GO                  TO   READ-000
     END-IF.
*
 FL-READ-EXIT.
     EXIT.
****************************************************************
*             終了処理                              3.0        *
****************************************************************
 END-SEC                SECTION.
*    ファイルのＣＬＯＳＥ
     CLOSE    HDENJNL   UHACHUD    DSPFILE   HTOKMS.
*
 END-EXIT.
     EXIT.
******************************************************************
 END PROGRAM  SKY1501B.
******************************************************************

```
