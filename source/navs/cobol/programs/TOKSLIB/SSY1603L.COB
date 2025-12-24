****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　島忠オンライン　　　　            *
*    モジュール名　　　　：　検収書発行　　　　　　　　　　    *
*    作成日／更新日　　　：　04/02/02                          *
*    作成者／更新者　　　：　ＮＡＶ　　　                      *
*    処理概要　　　　　　：　検収データより検収書を発行する。　*
*    伝票フォーマット　　：　ストックホーム                    *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SSY1603L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          04/02/02.
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
         YA        IS   YA
         YB        IS   YB
         YB-22     IS   YB22.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*    検収データＦ
     SELECT   SIMAKENS           ASSIGN    TO   SIMAKENS
                                 ACCESS    MODE IS   SEQUENTIAL
                                 FILE STATUS    IS   KEN-STATUS.
*    店舗マスタ
     SELECT   HTENMS             ASSIGN    TO   DA-01-VI-TENMS1
                                 ORGANIZATION   IS   INDEXED
                                 ACCESS    MODE IS   RANDOM
                                 RECORD    KEY  IS   TEN-F52
                                                     TEN-F011
                                 FILE STATUS    IS   TEN-STATUS.
*    画面ファイル
     SELECT   DSPFILE            ASSIGN    TO        GS-DSPF
                                 FORMAT              DSP-FMT
                                 GROUP               DSP-GRP
                                 PROCESSING          DSP-PRO
                                 FUNCTION            DSP-FNC
                                 STATUS              DSP-STATUS.
*    プリントファイル
     SELECT   PRINTF             ASSIGN    TO        LP-04.
*--------------------------------------------------------------*
 DATA                   DIVISION.
*--------------------------------------------------------------*
 FILE                   SECTION.
*****＜伝票データＦ＞*****
*****<< 島忠検収データ >>*****
 FD  SIMAKENS
                        BLOCK CONTAINS  25   RECORDS
                        LABEL RECORD    IS   STANDARD.
                        COPY  SIMAKENS  OF   XFDLIB
                        JOINING  KEN    AS   PREFIX.
*****<< 店舗マスタ >>*****
 FD  HTENMS             BLOCK CONTAINS   8   RECORDS
                        LABEL RECORD    IS   STANDARD.
                        COPY  HTENMS    OF   XFDLIB
                        JOINING   TEN   AS   PREFIX.
*
*****<< 画面　ファイル >>*****
 FD  DSPFILE            LABEL RECORD    IS   OMITTED.
*
     COPY    FSY16031   OF   XMDLIB.
*
*****＜プリント　ファイル＞*****
 FD  PRINTF                       LINAGE    IS   66   LINES.
 01  PRT-REC                      PIC       X(200).
*
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
***  ｽﾃｰﾀｽ ｴﾘｱ
 01  STATUS-AREA.
     03  KEN-STATUS               PIC  X(02).
     03  PRT-STATUS               PIC  X(02).
     03  TEN-STATUS               PIC  X(02).
***  ｶﾞﾒﾝ ｺﾝﾄﾛｰﾙ ｴﾘｱ
 01  DSP-CNTL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
     03  DSP-STATUS                   PIC  X(02).
***  ﾌﾗｸﾞ ｴﾘｱ
 01  FLG-AREA.
     03  END-FLG                  PIC  X(03)     VALUE   ZERO.
     03  SYU-FLG                  PIC  X(03)     VALUE   ZERO.
     03  ERR-FLG                  PIC  9(01)     VALUE   ZERO.
     03  SYORI-FLG                PIC  X(03)     VALUE   SPACE.
     03  KAI-FLG                  PIC  9(01)     VALUE   ZERO.
***  ｶｳﾝﾄ ｴﾘｱ
 01  CNT-AREA.
     03  L-CNT                    PIC  9(02)     VALUE   ZERO.
     03  CNT-AFTER                PIC  9(02)     VALUE   ZERO.
     03  CNT-GYO                  PIC  9(02)     VALUE   ZERO.
     03  CNT-DENPYO               PIC  9(09)     VALUE   ZERO.
     03  READ-CNT                 PIC  9(07)     VALUE   ZERO.
     03  IX                       PIC  9(01)     VALUE   ZERO.
     03  IY                       PIC  9(02)     VALUE   ZERO.
***  日付エリア
 01  WK-SYS-DATE.
     03  WK-YY1                   PIC  9(02)     VALUE   ZERO.
     03  WK-YMD.
         05  WK-YY2               PIC  9(02)     VALUE   ZERO.
         05  WK-MM                PIC  9(02)     VALUE   ZERO.
         05  WK-DD                PIC  9(02)     VALUE   ZERO.
 01  WK-DATE                      PIC  9(08)     VALUE   ZERO.
 01  CNT-PAGE                     PIC  9(04)     VALUE   ZERO.
 01  WK-KEN-F01                   PIC  9(07)     VALUE   ZERO.
 01  WK-KEN-F03                   PIC  9(03)     VALUE   ZERO.
 01  WK-DENPYO-KEI                PIC  9(09)     VALUE   ZERO.
***  ﾜｰｸ  ｴﾘｱ
 01  WRK-AREA.
     03  WRK-MAI                  PIC  9(06)     VALUE   ZERO.
     03  RD-SW                    PIC  9(01)     VALUE   ZERO.
     03  PAGE-SW                  PIC  9(01)     VALUE   ZERO.
***  ﾜｰｸ  ｴﾘｱ
 01  WK-HIDUKE-HEN.
     03  WK-HIDUKE-YY             PIC  X(04)     VALUE   SPACE.
     03  WK-HIDUKE-KU1            PIC  X(01)     VALUE   SPACE.
     03  WK-HIDUKE-MM             PIC  X(02)     VALUE   SPACE.
     03  WK-HIDUKE-KU2            PIC  X(01)     VALUE   SPACE.
     03  WK-HIDUKE-DD             PIC  X(02)     VALUE   SPACE.
*    ﾒﾂｾ-ｼﾞ ｴﾘｱ
 01  MSG-AREA.
     03  MSG-FIELD.
         05  MSG01                PIC  N(30)     VALUE
         NC"島忠検収書データが存在しません。".
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
         NC"島忠検収データ異常！！".
 01  FILE-ERR040                  PIC  N(11)     VALUE
         NC"画面ファイル　異常！！".
 01  FILE-ERR050                  PIC  N(11)     VALUE
         NC"店舗マスタ　　異常！！".
****  メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SSY1603L".
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
     03  FILLER                   PIC  X(08)     VALUE "SSY1603L".
     03  FILLER                   PIC  X(38)     VALUE   SPACE.
     03  FILLER                   PIC  N(13)     VALUE
         NC"＜　島　忠　検　収　書　＞"
         CHARACTER  TYPE  IS  YB22.
     03  FILLER                   PIC  X(26)     VALUE   SPACE.
     03  SYS-DT-YY                PIC  9(04).
     03  FILLER                   PIC  N(01)     VALUE NC"年"
         CHARACTER  TYPE  IS  YA.
     03  SYS-DT-MM                PIC  9(02).
     03  FILLER                   PIC  N(01)     VALUE NC"月"
         CHARACTER  TYPE  IS  YA.
     03  SYS-DT-DD                PIC  9(02).
     03  FILLER                   PIC  N(01)     VALUE NC"日"
         CHARACTER  TYPE  IS  YA.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  PG-CNT                   PIC  ZZ9.
     03  FILLER                   PIC  N(01)     VALUE NC"頁"
         CHARACTER  TYPE  IS  YA.
*
 01  HEAD-02                      CHARACTER  TYPE  IS    YA.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  FILLER                   PIC  N(02)     VALUE NC"店舗".
*
 01  HEAD-03                      CHARACTER  TYPE  IS    YA.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  FILLER                   PIC  N(02)     VALUE
         NC"伝票".
     03  FILLER                   PIC  X(03)     VALUE  "NO.".
     03  FILLER                   PIC  X(03)     VALUE   SPACE.
     03  FILLER                   PIC  N(03)     VALUE
         NC"発注日".
     03  FILLER                   PIC  X(05)     VALUE   SPACE.
     03  FILLER                   PIC  N(03)     VALUE
         NC"納品日".
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  HEAD-03-1                OCCURS  6.
         05  FILLER               PIC  X(07)     VALUE   SPACE.
         05  FILLER               PIC  X(01)     VALUE  "<".
         05  FILLER               PIC  N(04)     VALUE
             NC"仕入金額".
         05  FILLER               PIC  X(01)     VALUE   ">".
*
 01  SEN01.
     03  SEN01-1                  OCCURS    136.
         05  FILLER               PIC  X(01)     VALUE "=".
*
 01  SEN02.
     03  SEN02-1                  OCCURS    136.
         05  FILLER               PIC  X(01)     VALUE "-".
*
 01  SEN03.
     03  FILLER                   PIC  X(31)     VALUE   SPACE.
     03  SEN03-1                  OCCURS    105.
         05  FILLER               PIC  X(01)     VALUE "-".
*
 01  BODY-01                      CHARACTER  TYPE  IS  YA.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  BD01-TENCD               PIC  999.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  BD01-TENNM               PIC  N(15).
*
 01  BODY-02.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  BD02-DENNO               PIC  9(07).
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  BD02-HATYUD              PIC  X(10).
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
     03  BD02-NOUHND              PIC  X(10).
     03  BODY-01-01               OCCURS    6.
         05  FILLER               PIC  X(02)     VALUE   SPACE.
         05  BD02-KUGIRI1         PIC  X(01)     VALUE   SPACE.
         05  BD02-GYO             PIC  9(02).
         05  BD02-KUGIRI2         PIC  X(01)     VALUE   SPACE.
         05  FILLER               PIC  X(01)     VALUE   SPACE.
         05  BD02-KINGAKU         PIC  --,---,--9.
*    ｺﾞｳｹｲ
 01  TAIL-01                      CHARACTER  TYPE  IS  YA.
     03  FILLER                   PIC  X(107)    VALUE   SPACE.
     03  FILLER                   PIC  N(06)     VALUE
                                               NC"仕入金額合計".
     03  TL01-GOKEI               PIC  ---,---,--9.
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
 000-PRINTF-ERR         SECTION.
     USE      AFTER     EXCEPTION   PROCEDURE    PRINTF.
     MOVE     "SIMAKENS"       TO   ERR-FL-ID.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     DISPLAY   FILE-ERR010   UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
 000-MAST-ERR           SECTION.
     USE      AFTER     EXCEPTION   PROCEDURE    SIMAKENS.
     MOVE     "SIMAKENS"       TO   ERR-FL-ID.
     MOVE      KEN-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     DISPLAY   FILE-ERR030   UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
 000-DSPFILE-ERR        SECTION.
     USE      AFTER     EXCEPTION   PROCEDURE    DSPFILE.
     MOVE     "DSPFFILE"       TO   ERR-FL-ID.
     MOVE      DSP-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     DISPLAY   FILE-ERR040   UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
 010-MAST-ERR           SECTION.
     USE      AFTER     EXCEPTION   PROCEDURE    HTENMS.
     MOVE     "HTENMS  "       TO   ERR-FL-ID.
     MOVE      TEN-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     DISPLAY   FILE-ERR050   UPON   CONS.
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
     OPEN     INPUT     SIMAKENS  HTENMS.
     OPEN     I-O       DSPFILE.
*    ワーク初期化
     MOVE     ZERO               TO   CNT-DENPYO.
     MOVE     SPACE              TO   END-FLG  SYU-FLG.
     MOVE     SPACE              TO   FSY16031.
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
     PERFORM  SIMAKENS-CNT-SEC  UNTIL  SYORI-FLG =  "END".
*
*    伝票_ゼロ件処理（エラーメッセージ出力）
 INIT002.
     IF    WRK-MAI  <=  0
           MOVE     1         TO     ERR-FLG
     END-IF.
*    ファイルＯＰＥＮ／ＣＬＯＳＥ
     CLOSE          SIMAKENS.
     OPEN   INPUT   SIMAKENS.
*    終了ＦＬＧ解除
     MOVE     SPACE            TO    SYU-FLG.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*    島忠検収データ読込み
****************************************************************
 SIMAKENS-READ-SEC      SECTION.
*
     READ     SIMAKENS  AT   END
              MOVE    "END"      TO     SYORI-FLG
              NOT  AT  END
              ADD      1         TO     READ-CNT
     END-READ.
*
 SIMAKENS-READ-EXIT.
     EXIT.
****************************************************************
*    店舗マスタ読込み
****************************************************************
 HTENMS-READ-SEC        SECTION.
*
     MOVE     72028              TO     TEN-F52.
     MOVE     KEN-F03            TO     TEN-F011.
     READ     HTENMS
              INVALID     MOVE ALL NC"＊"  TO  BD01-TENNM
              NOT INVALID MOVE TEN-F03     TO  BD01-TENNM
     END-READ.
*店舗ＣＤセット
     MOVE     KEN-F03            TO      BD01-TENCD.
*
 HTENMS-READ-EXIT.
     EXIT.
****************************************************************
*    島忠検収データ検収カウント
****************************************************************
 SIMAKENS-CNT-SEC       SECTION.
*****島忠検収データ読込み
     PERFORM  SIMAKENS-READ-SEC.
*
     IF  WK-KEN-F01  NOT =  KEN-F01
     AND SYORI-FLG       =  SPACE
         ADD    1         TO   WRK-MAI
         MOVE   KEN-F01   TO   WK-KEN-F01
     END-IF.
*
 SIMAKENS-CNT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                              2.0      *
****************************************************************
 MAIN-SEC               SECTION.
*    画面の初期化
     MOVE          SPACE     TO   SYORI-FLG.
     MOVE          SPACE     TO   FSY16031.
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
                   MOVE      ZERO      TO   READ-CNT
                   MOVE      99        TO   L-CNT
                   PERFORM   FL-READ-SEC
                             UNTIL     SYORI-FLG =   "END"
         WHEN      OTHER
                   MOVE      2         TO   ERR-FLG
     END-EVALUATE.
     IF       PAGE-SW   =    1
              MOVE      ZERO      TO        PAGE-SW
     END-IF.
*    プロリントファイルＣＬＯＳＥ
     CLOSE         PRINTF    SIMAKENS.
*    次対象の入力為，ＲＥＡＤスイッチＯＦＦ
     MOVE          ZERO      TO   RD-SW.
*    伝票データＦＯＰＥＮ
     OPEN          INPUT     SIMAKENS.
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
     MOVE    "FSY16031" TO        DSP-FMT.
*    画面表示
     WRITE    FSY16031.
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
     PERFORM  SIMAKENS-READ-SEC.
*
     IF       SYORI-FLG  =  "END"
              IF   READ-CNT  =  ZERO
                   MOVE     "END"      TO   SYORI-FLG
                   MOVE      1         TO   ERR-FLG
                   PERFORM   ERR-MSG-SEC
                   GO                  TO   FL-READ-EXIT
              END-IF
              MOVE   WK-DENPYO-KEI  TO      TL01-GOKEI
              WRITE  PRT-REC  FROM  TAIL-01 AFTER  1
              WRITE  PRT-REC  FROM  SEN02   AFTER  1
              GO                       TO   FL-READ-EXIT
     END-IF.
*    明細行印字
     PERFORM  MEISAI-WT-SEC.
*
 FL-READ-EXIT.
     EXIT.
****************************************************************
*             ヘッド部出力処理                      2.5.2      *
****************************************************************
 HEAD-WT-SEC            SECTION.
*
     IF        CNT-PAGE       >    ZERO
               MOVE      SPACE     TO   PRT-REC
               WRITE     PRT-REC   AFTER     PAGE
     END-IF.
*    ヘッド部項目初期化
     INITIALIZE                  HEAD-01
                                 HEAD-02
                                 HEAD-03.
*--- 項目セット
*    日付セット
     MOVE     WK-DATE(1:4)     TO      SYS-DT-YY.
     MOVE     WK-DATE(5:2)     TO      SYS-DT-MM.
     MOVE     WK-DATE(7:2)     TO      SYS-DT-DD.
*    頁セット
     ADD      1                TO      CNT-PAGE.
     MOVE     CNT-PAGE         TO      PG-CNT.
*    ヘッダー行印字
     WRITE  PRT-REC  FROM  HEAD-01  AFTER  3.
     WRITE  PRT-REC  FROM  SEN01    AFTER  2.
     WRITE  PRT-REC  FROM  HEAD-02  AFTER  1.
     WRITE  PRT-REC  FROM  HEAD-03  AFTER  1.
     WRITE  PRT-REC  FROM  SEN01    AFTER  1.
*    印字位置送り
*    MOVE     SPACE            TO     PRT-REC.
*    WRITE    PRT-REC          AFTER  1.
*    行カウンタ
     MOVE     8                TO     L-CNT.
*
 HEAD-WT-EXIT.
     EXIT.
****************************************************************
*             ヘッド部出力処理                      2.5.2      *
****************************************************************
 MEISAI-WT-SEC          SECTION.
*    行カウンターブレイクチェック
     IF  L-CNT    >  56
         IF (( KEN-F03  NOT =  WK-KEN-F03 )  OR
             ( KEN-F01  NOT =  WK-KEN-F01 )) AND
             ( CNT-PAGE     >  ZERO       )
             MOVE   WK-DENPYO-KEI  TO      TL01-GOKEI
             WRITE  PRT-REC  FROM  TAIL-01 AFTER  1
             WRITE  PRT-REC  FROM  SEN02   AFTER  1
*            行カウンタ
             ADD    2              TO      L-CNT
             MOVE   ZERO           TO      WK-DENPYO-KEI  IY
             MOVE   KEN-F01        TO      WK-KEN-F01
         END-IF
         MOVE        1     TO   KAI-FLG
         PERFORM  HEAD-WT-SEC
         PERFORM  HTENMS-READ-SEC
         WRITE  PRT-REC  FROM  BODY-01  AFTER  1
         MOVE     KEN-F03          TO      WK-KEN-F03
*        行カウンタ
         ADD    1              TO      L-CNT
     END-IF.
*店ＣＤブレイクチェック
     IF  KEN-F03  NOT =  WK-KEN-F03
         IF   READ-CNT  >  1
              MOVE   WK-DENPYO-KEI  TO      TL01-GOKEI
              WRITE  PRT-REC  FROM  TAIL-01 AFTER  1
              WRITE  PRT-REC  FROM  SEN02   AFTER  1
*             行カウンタ
              ADD    2              TO      L-CNT
         END-IF
         PERFORM  HTENMS-READ-SEC
         WRITE  PRT-REC  FROM  BODY-01  AFTER  1
         MOVE   KEN-F03        TO      WK-KEN-F03
         MOVE   KEN-F01        TO      WK-KEN-F01
         MOVE   ZERO           TO      WK-DENPYO-KEI  IY
*        行カウンタ
         ADD    1              TO      L-CNT
     END-IF.
*明細行初期化
     MOVE       SPACE          TO      BODY-01  BODY-02.
*伝票番号ブレイクチェック
     IF  KEN-F01  NOT =  WK-KEN-F01
         IF   READ-CNT  >  1
              MOVE   WK-DENPYO-KEI  TO      TL01-GOKEI
              WRITE  PRT-REC  FROM  TAIL-01 AFTER  1
              WRITE  PRT-REC  FROM  SEN03   AFTER  1
*             行カウンタ
              ADD    2              TO      L-CNT
         END-IF
         MOVE        ZERO           TO      WK-DENPYO-KEI IY
         MOVE        KEN-F01        TO      WK-KEN-F01
     END-IF.
*
     IF  KAI-FLG  =  1
     OR  KEN-F15  =  1
         MOVE    ZERO          TO   KAI-FLG
*        伝票番号
         MOVE    KEN-F01       TO   BD02-DENNO
*        発注日
         MOVE    KEN-F08(1:4)  TO   WK-HIDUKE-YY
         MOVE    "/"           TO   WK-HIDUKE-KU1
         MOVE    KEN-F08(5:2)  TO   WK-HIDUKE-MM
         MOVE    "/"           TO   WK-HIDUKE-KU2
         MOVE    KEN-F08(7:2)  TO   WK-HIDUKE-DD
         MOVE    WK-HIDUKE-HEN TO   BD02-HATYUD
*        納品日
         MOVE    KEN-F09(1:4)  TO   WK-HIDUKE-YY
         MOVE    "/"           TO   WK-HIDUKE-KU1
         MOVE    KEN-F09(5:2)  TO   WK-HIDUKE-MM
         MOVE    "/"           TO   WK-HIDUKE-KU2
         MOVE    KEN-F09(7:2)  TO   WK-HIDUKE-DD
         MOVE    WK-HIDUKE-HEN TO   BD02-NOUHND
     END-IF.
*明細テーブルセット
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 6
       IF  KEN-F141(IX)  =  "-" OR "+"
             ADD     1         TO   IY
             MOVE    "<"       TO   BD02-KUGIRI1(IX)
             MOVE    IY        TO   BD02-GYO(IX)
             MOVE    ">"       TO   BD02-KUGIRI2(IX)
             IF  KEN-F141(IX)  =  "-"
                 COMPUTE BD02-KINGAKU(IX) = KEN-F142(IX) * -1
                 COMPUTE WK-DENPYO-KEI = WK-DENPYO-KEI +
                       ( KEN-F142(IX) * -1 )
             ELSE
                 MOVE KEN-F142(IX) TO  BD02-KINGAKU(IX)
                 COMPUTE WK-DENPYO-KEI = WK-DENPYO-KEI +
                                         KEN-F142(IX)
             END-IF
       END-IF
     END-PERFORM.
*
     WRITE  PRT-REC  FROM  BODY-02  AFTER  1.
*    行カウンタ
     ADD    1                  TO     L-CNT.
*
 MEISAI-WT-EXIT.
     EXIT.
****************************************************************
*             終了処理                              3.0        *
****************************************************************
 END-SEC                SECTION.
*
*    ファイルのＣＬＯＳＥ
     CLOSE    SIMAKENS  DSPFILE.
*
 END-EXIT.
     EXIT.
