# SSY4903L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SSY4903L.COB`

## ソースコード

```cobol
****************************************************************
*         SYSTEM･･･ホーマック関東オンラインシステム            *
*        PG-NAME･･･物品受領書発行                              *
*          PG-ID･･･SSY4903L                                    *
*                                            DATE. 00.08.07    *
*                                              BY. Y-YOSHIDA   *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SSY4903L.
 AUTHOR.                Y-YOSHIDA.
 DATE-WRITTEN.          00/08/07.
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
     SELECT   CVCSG001           ASSIGN    TO   CVCSG001
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
     03  DEN-02                   PIC  9(01).
     03  DEN-03                   PIC  X(126).
*
*****＜画面　ファイル＞*****
 FD  DSPFILE            LABEL RECORD   IS   OMITTED.
*
     COPY    FSY39021   OF   XMDLIB.
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
     03  L-CNT                    PIC  9(02)     VALUE   ZERO.
     03  CNT-AFTER                PIC  9(02)     VALUE   ZERO.
     03  CNT-GYO                  PIC  9(02)     VALUE   ZERO.
     03  CNT-DENPYO               PIC  9(09)     VALUE   ZERO.
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
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SSY4903L".
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
     03  FILLER                   PIC  X(08)     VALUE "SSY4903L".
     03  FILLER                   PIC  X(25)     VALUE   SPACE.
     03  FILLER                   PIC  N(19)     VALUE
         NC"＊＊　ホーマック仙台　物品受領書　＊＊"
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
     03  FILLER                   PIC  N(13)     VALUE
         NC"社名：ホーマック仙台　".
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
     03  HD03-DENNO               PIC  9999999.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
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
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  FILLER                   PIC  N(05)     VALUE
         NC"直送区分：".
     03  HD03-CYOKUSO             PIC  9(02).
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
*
 01  BODY-01.
     03  FILLER                   PIC  X(05)     VALUE   SPACE.
     03  BD01-GYO                 PIC  Z9.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  BD01-SYOUHIN             PIC  X(20).
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  BD01-JANCD               PIC  X(13).
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  BD01-SURYO               PIC  Z,ZZ9.9.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  BD01-GENKA-TANKA         PIC  ZZZ,ZZ9.99.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  BD01-GENKA-KINGAKU       PIC  ZZZ,ZZZ,ZZ9.
     03  FILLER                   PIC  X(05)     VALUE   SPACE.
     03  BD01-BAIKA-TANKA         PIC  ZZZ,ZZ9.
     03  FILLER                   PIC  X(04)     VALUE   SPACE.
     03  BD01-BAIKA-KINGAKU       PIC  ZZZ,ZZZ,ZZ9.
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
*ヘッドレコード退避ワーク
 01  WK-DEPB-REC.
     03  WK-DEPB01          PIC  X(01).
     03  WK-DEPB02          PIC  9(01).
     03  WK-DEPB03          PIC  9(07).
     03  WK-DEPB04          PIC  9(06).
     03  WK-DEPB05          PIC  9(06).
     03  WK-DEPB06.
         05  WK-DEPB061     PIC  9(06).
         05  WK-DEPB062     PIC  9(02).
     03  WK-DEPB07.
         05  WK-DEPB071     PIC  X(20).
         05  WK-DEPB072     PIC  X(20).
     03  WK-DEPB08.
         05  WK-DEPB081     PIC  9(04).
         05  WK-DEPB082     PIC  X(02).
     03  WK-DEPB09          PIC  X(15).
     03  WK-DEPB10.
         05  WK-DEPB101     PIC  9(03).
         05  WK-DEPB102     PIC  X(01).
     03  WK-DEPB11          PIC  9(02).
     03  WK-DEPB12          PIC  9(04).
     03  WK-DEPB13          PIC  9(02).
     03  WK-DEPB14          PIC  9(01).
     03  WK-DEPB15          PIC  9(01).
     03  WK-DEPB16          PIC  X(23).
     03  WK-DEPB17          PIC  X(01).

*    明細レコード退避ワーク
 01  WK-DEPD-REC.
     03  WK-DEPD01          PIC  X(01).
     03  WK-DEPD02          PIC  9(01).
     03  WK-DEPD03.
         05  WK-DEPD031     PIC  9(07).
         05  WK-DEPD032     PIC  X(01).
     03  WK-DEPD04.
         05  WK-DEPD041     PIC  X(20).
         05  WK-DEPD042     PIC  X(20).
     03  WK-DEPD05          PIC  9(05)V9.
     03  WK-DEPD06          PIC  9(05)V9.
     03  WK-DEPD07          PIC  9(06)V99.
     03  WK-DEPD08          PIC  9(08).
     03  WK-DEPD09          PIC  9(06).
     03  WK-DEPD10          PIC  9(08).
     03  WK-DEPD11          PIC  X(13).
     03  WK-DEPD12          PIC  X(23).
*    合計レコード退避ワーク
 01  WK-DEPT-REC.
     03  WK-DEPT01          PIC  X(01).
     03  WK-DEPT02          PIC  9(01).
     03  WK-DEPT03          PIC  9(08).
     03  WK-DEPT04          PIC  9(08).
     03  WK-DEPT05          PIC  X(110).
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
     MOVE     SPACE              TO   FSY39021.
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
     IF       DEN-01    =   "H"
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
     MOVE          SPACE     TO   FSY39021.
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
     MOVE    "FSY39021" TO        DSP-FMT.
*    画面表示
     WRITE    FSY39021.
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
     IF       DEN-01    =   "H"
              MOVE      DEN-REC        TO   WK-DEPB-REC
              PERFORM   HEAD-WT-SEC
              GO        TO   DENP-WT-010
     END-IF.
     IF       DEN-01    =   "L"
              MOVE      DEN-REC        TO   WK-DEPD-REC
     ELSE
              GO        TO   DENP-WT-010
     END-IF.
*    行カウンタの判定
     IF       L-CNT     >=   23
              PERFORM   TAIL-WT-SEC
              PERFORM   HEAD-WT-SEC
     END-IF.
*    項目初期化
     INITIALIZE                  BODY-01
                                 BODY-02.
*    明細項目セット
*    行_
     ADD      1                  TO   CNT-GYO.
     MOVE     CNT-GYO            TO   BD01-GYO.
*    品名
     MOVE     WK-DEPD041         TO   BD01-SYOUHIN.
*    品名
     MOVE     WK-DEPD042         TO   BD02-KIKAKU.
*    ＪＡＮコード
     MOVE     WK-DEPD11          TO   BD01-JANCD.
*    商品コード
     MOVE     WK-DEPD031         TO   BD01-SYOCD.
*    数量
     MOVE     WK-DEPD05          TO   BD01-SURYO.
*    原価単価
     MOVE     WK-DEPD07          TO   BD01-GENKA-TANKA.
*    原価金額
     MOVE     WK-DEPD08          TO   BD01-GENKA-KINGAKU.
*    売価単価
     MOVE     WK-DEPD09          TO   BD01-BAIKA-TANKA.
*    売価金額
     MOVE     WK-DEPD10          TO   BD01-BAIKA-KINGAKU.
*
     WRITE    PRT-REC        FROM     BODY-01   AFTER     1.
     WRITE    PRT-REC        FROM     BODY-02   AFTER     1.
*
     ADD      2                  TO   L-CNT.
*
 DENP-WT-010.
*
     PERFORM  FL-READ-SEC.
     IF       SYORI-FLG      =   "END"
              GO   TO   DENP-WT-EXIT
     END-IF.
*
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
         NOT AT END
*             テイル印字
              IF   DEN-01    =   "T"
                   MOVE      DEN-REC        TO   WK-DEPT-REC
                   PERFORM   TAIL-WT-SEC
                   MOVE      ZERO           TO   CNT-GYO
                   GO        TO   DENP-WT-010
              END-IF
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
     MOVE     WK-DEPB081       TO      HD03-TENCD.
     MOVE     WK-DEPB09        TO      HD03-TENMEI.
*    伝票番号
     MOVE     WK-DEPB03        TO      HD03-DENNO.
*    伝票区分
     MOVE     WK-DEPB11        TO      HD03-DENKU.
*    発注日
     MOVE     WK-DEPB04(1:2)   TO      HD03-H-YY.
     MOVE     WK-DEPB04(3:2)   TO      HD03-H-MM.
     MOVE     WK-DEPB04(5:2)   TO      HD03-H-DD.
*    検収日
     MOVE     WK-DEPB05(1:2)   TO      HD03-K-YY.
     MOVE     WK-DEPB05(3:2)   TO      HD03-K-MM.
     MOVE     WK-DEPB05(5:2)   TO      HD03-K-DD.
*    伝票区分
     EVALUATE  WK-DEPB11
         WHEN  "01"
               MOVE  NC"仕入　　　"  TO  HD03-DENKU-MEI
         WHEN  "02"
               MOVE  NC"返品　　　"  TO  HD03-DENKU-MEI
         WHEN  "03"
               MOVE  NC"値引　　　"  TO  HD03-DENKU-MEI
         WHEN  OTHER
               MOVE  NC"＊＊＊＊＊"  TO  HD03-DENKU-MEI
     END-EVALUATE.
*    取引先
     MOVE     WK-DEPB061       TO      HD03-TORICD.
     MOVE     WK-DEPB062       TO      HD03-CYOKUSO.
     MOVE     WK-DEPB07        TO      HD03-TORIMEI.
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
              MOVE      1         TO        PAGE-SW
     ELSE
              MOVE      SPACE     TO        PRT-REC
              WRITE     PRT-REC   AFTER     8
*
              MOVE      SPACE     TO        PRT-REC
              WRITE     PRT-REC                       AFTER  4
              MOVE      ZERO      TO        PAGE-SW
     END-IF.
     WRITE    PRT-REC          FROM   HEAD-03    AFTER    3.
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
     MOVE   WK-DEPT03            TO   TL01.
*    売価金額合計
     MOVE   WK-DEPT04            TO   TL02.
*    テイル印字制御
     COMPUTE   CNT-AFTER   =     25  -  L-CNT.
*    テイル行の印字
     WRITE    PRT-REC          FROM   TAIL-01   AFTER   CNT-AFTER.
*    次頁へ改頁
     MOVE     SPACE            TO     PRT-REC.
***  IF       PAGE-SW   =    0
***           WRITE     PRT-REC       AFTER     PAGE
***  ELSE
***           WRITE     PRT-REC       AFTER     8
***  END-IF.
*    伝票枚数
     ADD      1                TO     CNT-DENPYO.
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

```
