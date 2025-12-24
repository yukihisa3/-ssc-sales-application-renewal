****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　トステムビバ新ＥＤＩ　　　　　　　*
*    業務名　　　　　　　：　トステムビバオンラインシステム　　*
*    モジュール名　　　　：　受領返品明細発行                  *
*    作成日／更新日　　　：　2010/04/05                        *
*    作成者／更新者　　　：　ＮＡＶ　大野　　　　　　　　　　　*
*    処理概要　　　　　　：　検収データＦより物品受領書を発行　*
*    伝票フォーマット　　：　汎用用紙                          *
*    作成日／更新日　　　：　2010/04/19                        *
*    作成者／更新者　　　：　ＮＡＶ　大野　　　　　　　　　　　*
*                  　　　：　レコード区分がＢ、Ｃ、Ｄ以外は読み*
*                            飛ばすように変更                  *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SSY5050L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          10/04/05.
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
*    SELECT   CVCSG001           ASSIGN    TO   TOSSHIJ
     SELECT   CVCSG001           ASSIGN    TO   TSMJYU
                                 ACCESS    MODE IS   SEQUENTIAL
                                 FILE STATUS    IS   DEN-STATUS.
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
 FD  CVCSG001
                        BLOCK CONTAINS      1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  DEN-REC.
     03  DEN-01                   PIC  X(01).
     03  DEN-02                   PIC  X(02).
     03  DEN-03                   PIC  X(125).
*
*****＜画面　ファイル＞*****
 FD  DSPFILE            LABEL RECORD   IS   OMITTED.
*
     COPY    FSY50501   OF   XMDLIB.
*
*****＜プリント　ファイル＞*****
 FD  PRINTF                       LINAGE    IS   66   LINES.
 01  PRT-REC                      PIC       X(200).
*
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
***  ｽﾃｰﾀｽ ｴﾘｱ
 01  STATUS-AREA.
     03  DEN-STATUS               PIC  X(02).
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
     03  L-CNT1                   PIC  9(02)     VALUE   ZERO.
     03  L-CNT2                   PIC  9(02)     VALUE   ZERO.
     03  CNT-AFTER                PIC  9(02)     VALUE   ZERO.
     03  CNT-GYO                  PIC  9(02)     VALUE   ZERO.
     03  CNT-DENPYO               PIC  9(09)     VALUE   ZERO.
***  伝票合計計算
 01  WK-GOKEI.
     03  WK-GOKEI-GEN             PIC  9(09)     VALUE   ZERO.
     03  WK-GOKEI-BAI             PIC  9(09)     VALUE   ZERO.
***  日付エリア
 01  WK-SYS-DATE.
     03  WK-YY1                   PIC  9(02)     VALUE   ZERO.
     03  WK-YMD.
         05  WK-YY2               PIC  9(02)     VALUE   ZERO.
         05  WK-MM                PIC  9(02)     VALUE   ZERO.
         05  WK-DD                PIC  9(02)     VALUE   ZERO.
 01  WK-DATE                      PIC  9(08)     VALUE   ZERO.
 01  CNT-PAGE                     PIC  9(04)     VALUE   ZERO.
***  ﾜｰｸ  ｴﾘｱ
 01  WRK-AREA.
     03  WRK-MAI                  PIC  9(06)     VALUE   ZERO.
     03  RD-SW                    PIC  9(01)     VALUE   ZERO.
     03  PAGE-SW                  PIC  9(01)     VALUE   ZERO.
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
****  メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SSY5050L".
       03  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
***  ﾌﾟﾘﾝﾄ ｴﾘｱ
*    ﾐﾀﾞｼ
 01  HEAD-01.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  FILLER                   PIC  X(08)     VALUE "SSY5050L".
     03  FILLER                   PIC  X(25)     VALUE   SPACE.
     03  FILLER                   PIC  N(22)     VALUE
         NC"＊＊　トステムビバ　物品受領書（返品）　＊＊"
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
     03  FILLER                   PIC  N(12)     VALUE
         NC"社名：トステムビバ（株）".
*
 01  HEAD-03                      CHARACTER  TYPE  IS  PICH-YA.
     03  FILLER                   PIC  X(05)     VALUE   SPACE.
     03  FILLER                   PIC  N(03)     VALUE
         NC"店名：".
     03  HD03-TENCD               PIC  9(04).
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  HD03-TENMEI              PIC  X(15).
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  FILLER                   PIC  N(05)     VALUE
         NC"伝票番号：".
     03  HD03-DENNO               PIC  999999.
     03  FILLER                   PIC  X(03)     VALUE   SPACE.
     03  FILLER                   PIC  N(05)     VALUE
         NC"伝票区分：".
     03  HD03-DENKU               PIC  9(02).
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  HD03-DENKU-MEI           PIC  N(05).
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
         NC"受領日：".
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
     03  HD03-TORICD              PIC  9(05).
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  HD03-TORIMEI             PIC  X(30).
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  FILLER                   PIC  N(05)     VALUE
         NC"分類ＣＤ：".
     03  HD03-BUNRUI              PIC  9(04).
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  FILLER                   PIC  N(05)     VALUE
         NC"定特区分：".
     03  HD03-TEITOKU             PIC  X(01).
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  HD03-TEITOKU-MEI         PIC  N(02).
*
 01  HEAD-04                      CHARACTER  TYPE  IS  PICH-YA.
     03  FILLER                   PIC  X(05)     VALUE   SPACE.
     03  FILLER                   PIC  N(01)     VALUE
         NC"行".
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  FILLER                   PIC  N(04)     VALUE
         NC"商品名称".
     03  FILLER                   PIC  X(19)     VALUE   SPACE.
     03  FILLER                   PIC  N(05)     VALUE
         NC"商品コード".
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  FILLER                   PIC  N(03)     VALUE
         NC"発注数".
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  FILLER                   PIC  N(04)     VALUE
         NC"返品数".
     03  FILLER                   PIC  X(03)     VALUE   SPACE.
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
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
*    03  FILLER                   PIC  N(04)     VALUE
*        NC"欠品数量".
     03  FILLER                   PIC  X(05)     VALUE   SPACE.
     03  FILLER                   PIC  N(04)     VALUE
         NC"訂正区分".
*
 01  BODY-01.
     03  FILLER                   PIC  X(05)     VALUE   SPACE.
     03  BD01-GYO                 PIC  Z9.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  BD01-SYOUHIN-UE          PIC  X(25).
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  BD01-SYOCD               PIC  X(08).
     03  FILLER                   PIC  X(03)     VALUE   SPACE.
     03  BD01-SURYO               PIC  ZZ,ZZ9.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  BD01-HENPI-M             PIC  X(01)     VALUE   SPACE.
     03  BD01-HENPI                PIC  ZZ,ZZ9.
     03  BD01-HENPI-E             PIC  X(01)     VALUE   SPACE.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  BD01-GENKA-TANKA         PIC  ZZZ,ZZ9.99.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  BD01-GENKA-KINGAKU       PIC  ZZZ,ZZZ,ZZ9.
     03  FILLER                   PIC  X(06)     VALUE   SPACE.
     03  BD01-BAIKA-TANKA         PIC  ZZZ,ZZ9.
     03  FILLER                   PIC  X(04)     VALUE   SPACE.
     03  BD01-BAIKA-KINGAKU       PIC  ZZZ,ZZZ,ZZ9.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
*    03  BD01-KEPIN-M             PIC  X(01)     VALUE   SPACE.
*    03  BD01-KEPIN               PIC  Z,ZZ9.9.
*    03  BD01-KEPIN-E             PIC  X(01)     VALUE   SPACE.
     03  FILLER                   PIC  X(10)     VALUE   SPACE.
     03  BD01-TORIKESI            PIC  X(01)     VALUE   SPACE.
*
 01  BODY-02.
     03  FILLER                   PIC  X(09)     VALUE   SPACE.
     03  BD02-SYOUHIN-SITA        PIC  X(15).
     03  FILLER                   PIC  X(07)     VALUE   SPACE.
     03  BD02-JANCD               PIC  9(13).
*    ｺﾞｳｹｲ
 01  TAIL-01.
     03  FILLER                   PIC  X(76)     VALUE   SPACE.
     03  TL01                     PIC  ZZZ,ZZZ,ZZ9.
     03  FILLER                   PIC  X(17)     VALUE   SPACE.
     03  TL02                     PIC  ZZZ,ZZZ,ZZ9.
*
*伝票ヘッダーレコード（１）
 01  WK-DEPB-REC.
     03  WK-DEPB01          PIC  X(01).
     03  WK-DEPB02          PIC  X(02).
     03  WK-DEPB03          PIC  9(06).
     03  WK-DEPB04          PIC  9(01).
     03  WK-DEPB05          PIC  9(03).
     03  WK-DEPB06          PIC  9(04).
     03  WK-DEPB07          PIC  9(04).
     03  WK-DEPB08          PIC  9(02).
     03  WK-DEPB09          PIC  9(05).
     03  WK-DEPB10          PIC  X(06).
     03  WK-DEPB11          PIC  X(06).
     03  WK-DEPB12          PIC  X(03).
     03  WK-DEPB13          PIC  X(20).
     03  WK-DEPB14          PIC  X(20).
     03  WK-DEPB15          PIC  X(20).
     03  WK-DEPB16          PIC  X(10).
     03  WK-DEPB17          PIC  X(15).

*伝票ヘッダレコード（２）
 01  WK-DEPC-REC.
     03  WK-DEPC01          PIC  X(01).
     03  WK-DEPC02          PIC  X(02).
     03  WK-DEPC03          PIC  X(01).
     03  WK-DEPC04          PIC  X(30).
     03  WK-DEPC05          PIC  X(20).
     03  WK-DEPC06          PIC  X(20).
     03  WK-DEPC07          PIC  X(17).
     03  WK-DEPC08          PIC  X(17).
     03  WK-DEPC09          PIC  X(12).
     03  WK-DEPC10          PIC  X(01).
     03  WK-DEPC11          PIC  X(04).
     03  WK-DEPC12          PIC  X(03).
*伝票明細レコード
 01  WK-DEPD-REC.
     03  WK-DEPD01          PIC  X(01).
     03  WK-DEPD02          PIC  X(02).
     03  WK-DEPD03          PIC  9(02).
     03  WK-DEPD04          PIC  X(08).
     03  WK-DEPD05          PIC  X(02).
     03  WK-DEPD06          PIC  9(03).
     03  WK-DEPD07          PIC  9(05).
     03  WK-DEPD08          PIC  X(03).
     03  WK-DEPD09          PIC  9(05).
     03  WK-DEPD10          PIC  9(01).
     03  WK-DEPD11          PIC  9(07)V9(02).
     03  WK-DEPD12          PIC  9(07).
     03  WK-DEPD13          PIC  9(09).
     03  WK-DEPD14          PIC  9(09).
     03  WK-DEPD15          PIC  X(25).
     03  WK-DEPD16          PIC  X(15).
     03  WK-DEPD17          PIC  9(13).
     03  WK-DEPD18          PIC  9(05).
     03  WK-DEPD19          PIC  X(01).
     03  WK-DEPD20          PIC  X(01).
     03  WK-DEPD21          PIC  X(02).
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN            PIC X(01).
 01  LINK-IN-YMD6           PIC 9(06).
 01  LINK-IN-YMD8           PIC 9(08).
 01  LINK-OUT-RET           PIC X(01).
 01  LINK-OUT-YMD           PIC 9(08).
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
     USE      AFTER     EXCEPTION   PROCEDURE    PRINTF.
     MOVE     "CVCSG001"       TO   ERR-FL-ID.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     DISPLAY   FILE-ERR010   UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
*--- << ｼｲﾚ ﾃﾞ-ﾀ ｴﾗ- >> ---*
 000-MAST-ERR           SECTION.
     USE      AFTER     EXCEPTION   PROCEDURE    CVCSG001.
     MOVE     "CVCSG001"       TO   ERR-FL-ID.
     MOVE      DEN-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     DISPLAY   FILE-ERR030   UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
*--- << ｶﾞﾒﾝ    ｴﾗ- >> ---*
 000-DSPFILE-ERR        SECTION.
     USE      AFTER     EXCEPTION   PROCEDURE    DSPFILE.
     MOVE     "DSPFFILE"       TO   ERR-FL-ID.
     MOVE      DSP-ST          TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     DISPLAY   FILE-ERR040   UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
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
     OPEN     INPUT     CVCSG001.
     OPEN     I-O       DSPFILE.
*    ワーク初期化
     MOVE     ZERO               TO   CNT-DENPYO.
     MOVE     SPACE              TO   END-FLG.
     MOVE     SPACE              TO   FSY50501.
*    システム日付取得
     ACCEPT   WK-YMD    FROM     DATE.
     MOVE     "3"                TO   LINK-IN-KBN.
     MOVE     WK-YMD             TO   LINK-IN-YMD6.
     MOVE     ZERO               TO   LINK-IN-YMD8.
     MOVE     ZERO               TO   LINK-OUT-RET.
     MOVE     ZERO               TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"      USING   LINK-IN-KBN
                                      LINK-IN-YMD6
                                      LINK-IN-YMD8
                                      LINK-OUT-RET
                                      LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD       TO   WK-DATE.
*
*    伝票枚数カウント
 INIT001.
     READ     CVCSG001  AT   END
              GO        TO       INIT002
     END-READ.
*    伝票_ブレイクチェック
     IF       DEN-01    =   "B"
              ADD       1    TO    WRK-MAI
              GO             TO    INIT001
     ELSE
              GO             TO    INIT001
     END-IF.
*    伝票_ゼロ件処理（エラーメッセージ出力）
 INIT002.
     IF    WRK-MAI  <=  0
           MOVE     MSG01     TO     MD05
     END-IF.
*    ファイルＯＰＥＮ／ＣＬＯＳＥ
     CLOSE          CVCSG001.
     OPEN   INPUT   CVCSG001.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                              2.0      *
****************************************************************
 MAIN-SEC               SECTION.
*    画面の初期化
     MOVE          SPACE     TO   SYORI-FLG.
     MOVE          SPACE     TO   FSY50501.
*    画面表示
     PERFORM       ERR-MSG-SEC.
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
          GO       TO   MAIN-SEC
     END-IF.
*
     PERFORM       ERR-MSG-SEC.
     PERFORM       DSP-WRITE-SEC.
*    プリントファイルのＯＰＥＮ
     OPEN     OUTPUT    PRINTF.
*    処理判定
     EVALUATE      MD04
*        全プリント
         WHEN      1
                   MOVE      3         TO   ERR-FLG
                   PERFORM   ERR-MSG-SEC
                   PERFORM   DSP-WRITE-SEC
                   PERFORM   FL-READ-SEC
                   PERFORM   DENP-WT-SEC
                             UNTIL     SYORI-FLG =   "END"
         WHEN      OTHER
                   MOVE      2         TO   ERR-FLG
     END-EVALUATE.
     IF       PAGE-SW   =    1
              MOVE      ZERO      TO        PAGE-SW
     END-IF.
*    プロリントファイルＣＬＯＳＥ
     CLOSE         PRINTF    CVCSG001.
*    次対象の入力為，ＲＥＡＤスイッチＯＦＦ
     MOVE          ZERO      TO   RD-SW.
*    伝票データＦＯＰＥＮ
     OPEN          INPUT     CVCSG001.
*
     IF       ERR-FLG   =    ZERO
              MOVE     "END"      TO   END-FLG
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
*    伝票総枚数セット
     MOVE     WRK-MAI   TO        MD01.
     MOVE     SPACE     TO        DSP-CNTL.
     MOVE    "SCREEN"   TO        DSP-GRP.
     MOVE    "FSY50501" TO        DSP-FMT.
*    画面表示
     WRITE    FSY50501.
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
*             伝票出力処理                          2.5        *
****************************************************************
 DENP-WT-SEC            SECTION.
*    ヘッダ印字
     IF       DEN-01    =   "B"
*             ヘッダＢレコードの初期化
              INITIALIZE                    WK-DEPB-REC
*
              MOVE      DEN-REC        TO   WK-DEPB-REC
              IF  CNT-PAGE  >  ZERO
                  PERFORM   TAIL-WT-SEC
              END-IF
************* PERFORM   HEAD-WT-SEC
              MOVE      ZERO           TO   CNT-GYO
              GO        TO   DENP-WT-010
     END-IF.
     IF       DEN-01    =   "C"
*             ヘッダＣレコードの初期化
              INITIALIZE                    WK-DEPC-REC
*
              MOVE      DEN-REC        TO   WK-DEPC-REC
              PERFORM   HEAD-WT-SEC
              GO        TO   DENP-WT-010
     END-IF.
*    項目初期化
     INITIALIZE                  BODY-01
                                 BODY-02.
*    明細項目レコードの初期化
     INITIALIZE                       WK-DEPD-REC.
*    明細項目セット
     MOVE     DEN-REC            TO   WK-DEPD-REC.
*    行_
     ADD      1                  TO   CNT-GYO.
     MOVE     CNT-GYO            TO   BD01-GYO.
*    品名
     MOVE     WK-DEPD15          TO   BD01-SYOUHIN-UE.
*    品名
     MOVE     WK-DEPD16          TO   BD02-SYOUHIN-SITA.
*    商品ＣＤ
     MOVE     WK-DEPD04          TO   BD01-SYOCD.
*    ＪＡＮＣＤ
     MOVE     WK-DEPD17          TO   BD02-JANCD.
*    発注数量
     MOVE     WK-DEPD09          TO   BD01-SURYO.
*    実納品数量
     MOVE     "("                TO   BD01-HENPI-M.
     MOVE     WK-DEPD18          TO   BD01-HENPI.
     MOVE     ")"                TO   BD01-HENPI-E.
*    原価単価
     MOVE     WK-DEPD11          TO   BD01-GENKA-TANKA.
*    原価金額
     COMPUTE  BD01-GENKA-KINGAKU =   WK-DEPD18 * WK-DEPD11.
     COMPUTE WK-GOKEI-GEN = WK-GOKEI-GEN + WK-DEPD18 * WK-DEPD11.
*売価単価
     MOVE     WK-DEPD12          TO   BD01-BAIKA-TANKA.
*    売価金額
     COMPUTE  BD01-BAIKA-KINGAKU =   WK-DEPD18 * WK-DEPD12.
     COMPUTE WK-GOKEI-BAI = WK-GOKEI-BAI + WK-DEPD18 * WK-DEPD12.
*    欠品数量
*    MOVE     "("                TO   BD01-KEPIN-M.
*    COMPUTE  BD01-KEPIN  =  WK-DEPD09 - WK-DEPD18.
*    MOVE     ")"                TO   BD01-KEPIN-E.
*    取消区分
     MOVE     WK-DEPD19          TO   BD01-TORIKESI.
*
     WRITE    PRT-REC        FROM     BODY-01   AFTER     1.
     WRITE    PRT-REC        FROM     BODY-02   AFTER     1.
*
     ADD      2                  TO   L-CNT1.
     ADD      2                  TO   L-CNT2.
*
 DENP-WT-010.
*
     PERFORM  FL-READ-SEC.
     IF       SYORI-FLG      =   "END"
              PERFORM   TAIL-WT-SEC
     END-IF.
*
 DENP-WT-EXIT.
     EXIT.
****************************************************************
*             ファイルＲＥＡＤ処理                  2.5.1      *
****************************************************************
 FL-READ-SEC            SECTION.
*    伝票データ読込み
 READ-000.
     READ     CVCSG001  AT        END
              MOVE     "END"      TO   SYORI-FLG
              MOVE      1         TO   ERR-FLG
              PERFORM   ERR-MSG-SEC
              GO                  TO   FL-READ-EXIT
*
*             2010/04/19 ADD STR
*             レコード区分がＢＣＤ以外は読み飛ばす（読み直す）
              NOT       AT        END
*
              EVALUATE  DEN-01
                   WHEN     "D"   CONTINUE
                   WHEN     "C"   CONTINUE
                   WHEN     "B"   CONTINUE
                   WHEN      OTHER
                        GO   TO   FL-READ-SEC
              END-EVALUATE
*             2010/04/19 ADD END
     END-READ.
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
     MOVE     WK-DATE(1:4)     TO      SYS-DT-YY.
     MOVE     WK-DATE(5:2)     TO      SYS-DT-MM.
     MOVE     WK-DATE(7:2)     TO      SYS-DT-DD.
*    店舗情報
     MOVE     WK-DEPB06        TO      HD03-TENCD.
     MOVE     WK-DEPB14        TO      HD03-TENMEI.
*    伝票番号
     MOVE     WK-DEPB03        TO      HD03-DENNO.
*    伝票区分
     MOVE     WK-DEPB08        TO      HD03-DENKU.
*    発注日
     MOVE     WK-DEPB10(1:2)   TO      HD03-H-YY.
     MOVE     WK-DEPB10(3:2)   TO      HD03-H-MM.
     MOVE     WK-DEPB10(5:2)   TO      HD03-H-DD.
*    受領日
     MOVE     WK-DEPB11(1:2)   TO      HD03-K-YY.
     MOVE     WK-DEPB11(3:2)   TO      HD03-K-MM.
     MOVE     WK-DEPB11(5:2)   TO      HD03-K-DD.
*    取引先
     MOVE     WK-DEPB09        TO      HD03-TORICD.
     MOVE     WK-DEPB15        TO      HD03-TORIMEI.
*    分類
     MOVE     WK-DEPB07        TO      HD03-BUNRUI.
*    定番特売区分
     MOVE     WK-DEPC03        TO      HD03-TEITOKU.
     EVALUATE WK-DEPC03
         WHEN "0"     MOVE NC"定番"  TO  HD03-TEITOKU-MEI
         WHEN "1"     MOVE NC"特売"  TO  HD03-TEITOKU-MEI
         WHEN "2"     MOVE NC"店特"  TO  HD03-TEITOKU-MEI
         WHEN SPACE   MOVE NC"手書"  TO  HD03-TEITOKU-MEI
         WHEN OTHER   MOVE NC"　　"  TO  HD03-TEITOKU-MEI
     END-EVALUATE.
*
     IF       PAGE-SW   =    0
              IF        CNT-PAGE       >    ZERO
                        MOVE      SPACE     TO   PRT-REC
                        WRITE     PRT-REC   AFTER     PAGE
              END-IF
*             頁セット
              ADD       1         TO        CNT-PAGE
              MOVE      CNT-PAGE  TO        PG-CNT
              WRITE     PRT-REC   FROM      HEAD-01   AFTER  4
              MOVE      SPACE     TO        PRT-REC
              WRITE     PRT-REC                       AFTER  2
              MOVE      1         TO        PAGE-SW
              MOVE      6         TO        L-CNT1
     ELSE
              MOVE      ZERO      TO        PAGE-SW
     END-IF.
     WRITE    PRT-REC          FROM   HEAD-03    AFTER    3.
     WRITE    PRT-REC          FROM   HEAD-031   AFTER    1.
     WRITE    PRT-REC          FROM   HEAD-04    AFTER    2.
*    印字位置送り
     MOVE     SPACE            TO     PRT-REC.
     WRITE    PRT-REC          AFTER  1.
*    行カウンタ
     ADD      6                TO     L-CNT1.
     MOVE     6                TO     L-CNT2.
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
     MOVE   WK-GOKEI-GEN         TO   TL01.
*    売価金額合計
     MOVE   WK-GOKEI-BAI         TO   TL02.
*    テイル印字制御
     COMPUTE   CNT-AFTER   =     25  -  L-CNT2.
*    テイル行の印字
     WRITE    PRT-REC          FROM   TAIL-01   AFTER   CNT-AFTER.
*    次頁へ改頁
     MOVE     SPACE            TO     PRT-REC.
*****DISPLAY "PAGE-SW = " PAGE-SW UPON CONS.
     IF       PAGE-SW   =    0
**************WRITE     PRT-REC       AFTER     PAGE
              CONTINUE
     ELSE
              WRITE     PRT-REC       AFTER     4
     END-IF.
*    伝票枚数
     ADD      1                TO     CNT-DENPYO.
*    伝票合計初期化
     MOVE     ZERO             TO     WK-GOKEI.
*****IF       PAGE-SW   =    1
*****         MOVE      ZERO      TO        PAGE-SW
*****END-IF.
*
 TAIL-WT-EXIT.
     EXIT.
****************************************************************
*             終了処理                              3.0        *
****************************************************************
 END-SEC                SECTION.
*
*    ファイルのＣＬＯＳＥ
     CLOSE    CVCSG001  DSPFILE.
*
 END-EXIT.
     EXIT.
