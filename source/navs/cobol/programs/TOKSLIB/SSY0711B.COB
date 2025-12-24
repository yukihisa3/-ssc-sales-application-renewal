****************************************************************
*  ＜量販店入力ルート用＞                                      *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    サブシステム　　　　：　出荷管理システム                  *
*    業務名　　　　　　　：　オンライン配送発注集計表　　　　　*
*    モジュール名　　　　：　商品毎数量集計                    *
*    作成日／更新日　　　：　2000/03/14                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　                                  *
*    再利用ＰＧ　　　　　：  VDA0530B.SKTSLIB                  *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SSY0711B.
 AUTHOR.                HAGIWARA.
 DATE-WRITTEN.          00/03/14.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM.
 OBJECT-COMPUTER.       FACOM.
 SPECIAL-NAMES.         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*
*****<< 配送伝票データ >>*****
     SELECT   INFILE    ASSIGN    TO        SHWHISF
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   IN-STATUS.
*
*****<< 配送伝票データ >>*****
     SELECT   OUTFILE   ASSIGN    TO        SHWHISGF
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   OUT-STATUS.
*
*----------------------------------------------------------------*
 DATA                   DIVISION.
*----------------------------------------------------------------*
 FILE                   SECTION.
*    FILE = 共通伝票データファイル
 FD  INFILE             BLOCK CONTAINS   1  RECORDS.
 01  IN-REC.
     03  IN01               PIC       9(08).
     03  IN02               PIC       9(09).
     03  IN03               PIC       9(02).
     03  IN04               PIC       9(01).
     03  IN05.
         05  IN051          PIC       9(02).
         05  IN052          PIC       N(04).
     03  IN06               PIC       9(02).
     03  IN07               PIC       9(05).
     03  IN08               PIC       X(02).
     03  IN09               PIC       X(02).
     03  IN10               PIC       X(07).
     03  IN11.
         05  IN111          PIC       9(08)     PACKED-DECIMAL.
         05  IN112          PIC       9(08)     PACKED-DECIMAL.
         05  IN113          PIC       9(08)     PACKED-DECIMAL.
     03  IN12               PIC       X(04).
     03  IN13.
         05  IN131          PIC       X(02).
         05  IN132          PIC       X(02).
         05  IN133          PIC       X(01).
         05  IN134          PIC       X(01).
     03  IN14.
         05  IN141.
             07  IN1411     PIC       X(08).
             07  IN1412     PIC       X(08).
         05  IN142.
             07  IN1421     PIC       X(15).
             07  IN1422     PIC       X(15).
     03  IN15               PIC      S9(09)V99  PACKED-DECIMAL.
     03  IN16               PIC       X(01).
     03  IN17.
         05  IN171          PIC      S9(09)V99  PACKED-DECIMAL.
         05  IN172          PIC      S9(09)V99  PACKED-DECIMAL.
         05  IN173          PIC      S9(09)V99  PACKED-DECIMAL.
     03  IN18.
         05  IN181          PIC      S9(11)     PACKED-DECIMAL.
         05  IN182          PIC      S9(11)     PACKED-DECIMAL.
     03  IN19               PIC      S9(09)     PACKED-DECIMAL.
     03  IN20               PIC      S9(09)     PACKED-DECIMAL.
     03  IN21               PIC       X(05).
     03  IN22               PIC       X(10).
     03  IN23               PIC       9(09).
     03  IN24               PIC       X(08).
     03  IN25               PIC       X(13).
     03  IN26               PIC       X(06).
     03  IN27               PIC       X(06).
     03  FILLER             PIC       X(29).
     03  IN98               PIC       9(02).
     03  IN99               PIC       9(02).
*    FILE = 配送伝票データワーク
 FD  OUTFILE            BLOCK CONTAINS   1  RECORDS.
 01  OUT-REC.
     03  OUT01              PIC       9(08).
     03  OUT02              PIC       9(09).
     03  OUT03              PIC       9(02).
     03  OUT04              PIC       9(01).
     03  OUT05.
         05  OUT051         PIC       9(02).
         05  OUT052         PIC       N(04).
     03  OUT06              PIC       9(02).
     03  OUT07              PIC       9(05).
     03  OUT08              PIC       X(02).
     03  OUT09              PIC       X(02).
     03  OUT10              PIC       X(07).
     03  OUT11.
         05  OUT111         PIC       9(08)     PACKED-DECIMAL.
         05  OUT112         PIC       9(08)     PACKED-DECIMAL.
         05  OUT113         PIC       9(08)     PACKED-DECIMAL.
     03  OUT12              PIC       X(04).
     03  OUT13.
         05  OUT131         PIC       X(02).
         05  OUT132         PIC       X(02).
         05  OUT133         PIC       X(01).
         05  OUT134         PIC       X(01).
     03  OUT14.
         05  OUT141.
             07  OUT1411    PIC       X(08).
             07  OUT1412    PIC       X(08).
         05  OUT142.
             07  OUT1421    PIC       X(15).
             07  OUT1422    PIC       X(15).
     03  OUT15              PIC      S9(09)V99  PACKED-DECIMAL.
     03  OUT16              PIC       X(01).
     03  OUT17.
         05  OUT171         PIC      S9(09)V99  PACKED-DECIMAL.
         05  OUT172         PIC      S9(09)V99  PACKED-DECIMAL.
         05  OUT173         PIC      S9(09)V99  PACKED-DECIMAL.
     03  OUT18.
         05  OUT181         PIC      S9(11)     PACKED-DECIMAL.
         05  OUT182         PIC      S9(11)     PACKED-DECIMAL.
     03  OUT19              PIC      S9(09)     PACKED-DECIMAL.
     03  OUT20              PIC      S9(09)     PACKED-DECIMAL.
     03  OUT21              PIC       X(05).
     03  OUT22              PIC       X(10).
     03  OUT23              PIC       9(09).
     03  OUT24              PIC       X(08).
     03  OUT25              PIC       X(13).
     03  OUT26              PIC       X(06).
     03  OUT27              PIC       X(06).
     03  FILLER             PIC       X(29).
     03  OUT98              PIC       9(02).
     03  OUT99              PIC       9(02).
*
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*----------------------------------------------------------------*
*    エンドフラグ
 01  END-FLG                      PIC       X(03)  VALUE SPACE.
*    ステイタス　エリア
 01  STATUS-AREA.
     03  IN-STATUS                PIC       X(02).
     03  OUT-STATUS               PIC       X(02).
*    ワークエリア
 01  WORK-AREA.
     03  WK-SYUKKA                PIC       X(02)  VALUE SPACE.
     03  WK-SYOCD1                PIC       X(16)  VALUE SPACE.
     03  WK-SYOCD2                PIC       X(13)  VALUE SPACE.
     03  WK-GOKEI                 PIC       9(06)  VALUE ZERO.
     03  WK-NOUDATE               PIC       9(08)  PACKED-DECIMAL.
     03  WK-RUTO                  PIC       9(02)  VALUE ZERO.
     03  IX                       PIC       9(03)  VALUE ZERO.
     03  IY                       PIC       9(03)  VALUE ZERO.
     03  WK-RECORD                PIC       X(256) VALUE SPACE.
     03  RD-CNT                   PIC       9(04)  VALUE ZERO.
*    ルート退避
 01  SAV-RUTO.
     03  RUTO                     PIC       9(02)  OCCURS 999.
*エラーセクション名
 01  SEC-NAME.
     03  FILLER         PIC  X(05)  VALUE " *** ".
     03  S-NAME         PIC  X(30).
*    メッセージ　エリア
 01  MSG-AREA.
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                      "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                      "SSY0711B".
         05  FILLER               PIC       X(10)  VALUE
                      " ABEND ###".
*
     03  MSG-ABEND2.
         05  FILLER               PIC       X(04)  VALUE
                      "### ".
         05  ERR-FL-ID            PIC       X(08).
         05  FILLER               PIC       X(04)  VALUE
                      " ST-".
         05  ERR-STCD             PIC       X(02).
         05  FILLER               PIC       X(04)  VALUE
                      " ###".
*
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE INFILE.
     MOVE     "INFILE"            TO        ERR-FL-ID.
     MOVE     IN-STATUS           TO        ERR-STCD.
     MOVE     4000                TO        PROGRAM-STATUS.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC2         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE OUTFILE.
     MOVE     "OUTFILE"           TO        ERR-FL-ID.
     MOVE     OUT-STATUS          TO        ERR-STCD.
     MOVE     4000                TO        PROGRAM-STATUS.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 END          DECLARATIVES.
*
******************************************************************
*                       SHORI                         0.0        *
******************************************************************
 CONTROL-START       SECTION.
     MOVE     "CONTROL-START"     TO   S-NAME.
*
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC       UNTIL  END-FLG = "END".
     PERFORM  END-SEC.
     STOP     RUN.
 CONTROL-END.
     EXIT.
****************************************************************
*       初期処理                                    1.0        *
****************************************************************
 INIT-SEC     SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*
*    ファイルのＯＰＥＮ
     OPEN     INPUT     INFILE.
     OPEN     OUTPUT    OUTFILE.
*
     MOVE     SPACE     TO       WK-SYUKKA WK-SYOCD1 WK-SYOCD2.
     MOVE     ZERO      TO       WK-GOKEI  WK-NOUDATE.
*    共通伝票データ初期ＲＥＡＤ
     PERFORM  INFILE-RD-SEC.
     MOVE     IN08      TO        WK-SYUKKA.
     MOVE     IN141     TO        WK-SYOCD1.
     MOVE     IN25      TO        WK-SYOCD2.
     MOVE     IN112     TO        WK-NOUDATE.
     MOVE     IN-REC    TO        WK-RECORD.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*       共通伝票データ読込み                        1.1        *
****************************************************************
 INFILE-RD-SEC         SECTION.
     MOVE     "INFILE-RD-SEC"     TO   S-NAME.
*
     READ     INFILE    AT        END
              MOVE     "END"      TO        END-FLG
              GO       TO     INFILE-RD-EXIT
              NOT  AT  END
              ADD      1          TO        RD-CNT
     END-READ.
*    出荷場所，ルート，ルート順が初期値の時，読飛ばし
     IF       IN08     =      SPACE
     OR       IN98     =      ZERO
     OR       IN99     =      ZERO
              GO       TO     INFILE-RD-SEC
     END-IF.
*
 INFILE-RD-EXIT.
     EXIT.
****************************************************************
*       メイン処理                                  2.0        *
****************************************************************
 MAIN-SEC     SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.
*
*    ブレイクチェック（納品日，出荷場所，商品コード）
 MAIN010.
     IF    WK-NOUDATE NOT =  IN112
     OR    WK-SYUKKA  NOT =  IN08
     OR    WK-SYOCD1  NOT =  IN141
     OR    WK-SYOCD2  NOT =  IN25
           PERFORM    GOKEI-WT-SEC
           MOVE  SPACE  TO   WK-SYUKKA WK-SYOCD1 WK-SYOCD2
           MOVE  ZERO   TO   WK-GOKEI
           MOVE  IN08   TO   WK-SYUKKA
           MOVE  IN141  TO   WK-SYOCD1
           MOVE  IN25   TO   WK-SYOCD2
           MOVE  IN112  TO   WK-NOUDATE
           MOVE  SPACE  TO   WK-RECORD
           MOVE  ZERO   TO   WK-RUTO
           MOVE  ZERO   TO   IX IY
           MOVE  ZERO   TO   SAV-RUTO
     END-IF.
 MAIN020.
*    ルート格納 (ルート毎に合計レコード作成の為）
     IF    WK-RUTO  NOT =  IN98
           ADD   1      TO   IX
***********DISPLAY "IX = " IX UPON CONS
           MOVE  IN98   TO   RUTO(IX)
           MOVE  IN98   TO   WK-RUTO
     END-IF.
 MAIN030.
*    数量集計
     ADD   IN15         TO   WK-GOKEI.
     MOVE  IN-REC       TO   WK-RECORD.
 MAIN040.
*    配送伝票データの読込み
     PERFORM     INFILE-RD-SEC.
*    終了時，合計レコード追加へ
     IF    END-FLG = "END"
           PERFORM    GOKEI-WT-SEC
     END-IF.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*       合計レコード追加                            2.1        *
****************************************************************
 GOKEI-WT-SEC          SECTION.
     MOVE     "GOKEI-WT-SEC"      TO   S-NAME.
*
*    合計レコード内容転送_追加
 GOKEI010.
     PERFORM VARYING IY FROM 1 BY 1 UNTIL IY > IX
             MOVE  SPACE       TO   OUT-REC
             INITIALIZE             OUT-REC
             MOVE  WK-RECORD   TO   OUT-REC
             MOVE  WK-SYUKKA   TO   OUT08
             MOVE  WK-SYOCD1   TO   OUT141
             MOVE  WK-SYOCD2   TO   OUT25
             MOVE  WK-GOKEI    TO   OUT15
             MOVE  RUTO(IY)    TO   OUT98
             MOVE  99          TO   OUT99
             WRITE OUT-REC
     END-PERFORM.
*
 GOKEI-WT-EXIT.
     EXIT.
****************************************************************
*       終了処理                                    3.0        *
****************************************************************
 END-SEC      SECTION.
     MOVE     "END-SEC"           TO   S-NAME.
*
*
     CLOSE    INFILE OUTFILE.
*
 END-EXIT.
     EXIT.
********************<<  PROGRAM  END  >>**************************
