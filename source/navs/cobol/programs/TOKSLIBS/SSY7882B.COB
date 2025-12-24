****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ベンダーオンライン　　　　　　　　*
*    モジュール名　　　　：　受領データ変換処理　　　　　　　　*
*    作成日／更新日　　　：　2008/11/13                        *
*    作成者／更新者　　　：　高橋　　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　（ＣＶＣＳ）オンラインデータから　*
*                            受領データを作成する。　　　　　　*
*                            ロイヤルＨＣ　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY7882B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          08/11/13.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*受信データファイル
     SELECT   ROYALJYR  ASSIGN    TO        DA-01-S-ROYALJYR
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    JJR-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*受領データファイル
     SELECT   ROYJYRL1  ASSIGN    TO        DA-01-VI-ROYJYRL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYR-F00   JYR-F01
                                            JYR-F02   JYR-F03
                                            JYR-F04   JYR-F05
                                            JYR-F06   JYR-F07
                        FILE  STATUS   IS   JYR-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    受信データ　ＲＬ＝　２５６　  ＢＦ＝　１
******************************************************************
 FD  ROYALJYR
                        BLOCK CONTAINS      1    RECORDS
                        LABEL RECORD   IS   STANDARD.
*
 01  DEN-REC.
     03  DEN-01                  PIC  X(02).
     03  DEN-02                  PIC  X(254).
******************************************************************
*    受領データ
******************************************************************
 FD  ROYJYRL1
                        LABEL RECORD   IS   STANDARD.
     COPY     ROYJYRF   OF        XFDLIB
              JOINING   JYR  AS   PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  IDX                     PIC  9(02)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  CNT-KENSU               PIC  9(08)     VALUE  ZERO.
 01  CNT-KENSU-D             PIC  9(08)     VALUE  ZERO.
 01  CNT-MAISU               PIC  9(08)     VALUE  ZERO.
 01  INV-RUT                 PIC  9(01)     VALUE  ZERO.
 01  FLG-TOK                 PIC  9(01)     VALUE  ZERO.
 01  WK-TOKCD                PIC  9(06)     VALUE  ZERO.
 01  WK-KEY                  PIC  X(01)     VALUE  SPACE.
 01  WK-KEY-FLG              PIC  X(01)     VALUE  SPACE.
 01  ROYJYRL1-INV-FLG        PIC  X(03)     VALUE  SPACE.
*ヘッドレコード退避ワーク
 01  WK-DEPB-REC.
     03  WK-DEPB01          PIC  X(02).
     03  WK-DEPB02          PIC  9(05).
     03  WK-DEPB03          PIC  9(03).
     03  WK-DEPB04          PIC  X(02).
     03  WK-DEPB05          PIC  9(08).
     03  WK-DEPB06          PIC  9(09).
     03  WK-DEPB07          PIC  9(08).
     03  WK-DEPB08          PIC  9(08).
     03  WK-DEPB09          PIC  9(08).
     03  WK-DEPB10          PIC  9(08).
     03  WK-DEPB11          PIC  9(08).
     03  WK-DEPB12          PIC  X(28).
     03  WK-DEPB13          PIC  9(01).
     03  WK-DEPB14          PIC  9(12).
     03  WK-DEPB15          PIC  9(09).
     03  WK-DEPB16          PIC  X(119).
     03  WK-DEPB17          PIC  9(11).
     03  WK-DEPB18          PIC  9(01).
     03  WK-DEPB19          PIC  9(06).
*    明細レコード退避ワーク
 01  WK-DEPD-REC.
     03  WK-DEPD01          PIC  X(02).
     03  WK-DEPD02          PIC  9(08).
     03  WK-DEPD03          PIC  9(02).
     03  WK-DEPD04          PIC  9(01).
     03  WK-DEPD05          PIC  X(13).
     03  WK-DEPD06          PIC  9(04).
     03  WK-DEPD07          PIC  9(08).
     03  WK-DEPD08          PIC  9(05).
     03  WK-DEPD09          PIC  9(08)V99.
     03  WK-DEPD10          PIC  9(09).
     03  WK-DEPD11          PIC  9(08)V99.
     03  WK-DEPD12          PIC  9(09).
     03  WK-DEPD13          PIC  9(05).
     03  WK-DEPD14          PIC  9(08)V99.
     03  WK-DEPD15          PIC  9(09).
     03  WK-DEPD16          PIC  X(100).
     03  WK-DEPD17          PIC  X(33).
     03  WK-DEPD18          PIC  9(11).
     03  WK-DEPD19          PIC  9(01).
     03  WK-DEPD20          PIC  9(06).
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  JJR-STATUS        PIC  X(02).
     03  JYR-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY7882B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY7882B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY7882B".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ROYALJYR.
     MOVE      "ROYALJYR"   TO   AB-FILE.
     MOVE      JJR-STATUS  TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ROYJYRL1.
     MOVE      "ROYJYRL1"   TO   AB-FILE.
     MOVE      JYR-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FG    =    9.
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     ROYALJYR.
     OPEN     I-O       ROYJYRL1.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
     MOVE     SPACE     TO        WK-DEPB-REC.
     INITIALIZE                   WK-DEPB-REC.
     MOVE     SPACE     TO        WK-DEPD-REC.
     INITIALIZE                   WK-DEPD-REC.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*
     READ     ROYALJYR
              AT END    MOVE      9         TO  END-FG
              NOT AT END
                        ADD       1         TO  RD-CNT
     END-READ.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*
     MOVE     DEN-01             TO   WK-KEY.
*    伝票ヘッダレコード
     IF    DEN-01  =  "DH"
           MOVE      SPACE    TO    WK-DEPB-REC
           INITIALIZE               WK-DEPB-REC
           MOVE      DEN-REC  TO    WK-DEPB-REC
     END-IF.
*明細行
     IF    DEN-01  =  "DD"
           MOVE      SPACE    TO   WK-DEPD-REC
           INITIALIZE              WK-DEPD-REC
           MOVE      DEN-REC  TO   WK-DEPD-REC
           PERFORM   EDIT-SEC
     END-IF.
*
     READ     ROYALJYR
              AT END
              MOVE     9      TO    END-FG
              GO              TO    MAIN-EXIT
              NOT AT END
              ADD      1      TO    RD-CNT
     END-READ.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ファイル出力　　　　　　　　　　　　　　　　　　*
****************************************************************
 EDIT-SEC              SECTION.
*
     MOVE    "EDIT-SEC"     TO        S-NAME.
     MOVE     SPACE         TO        JYR-REC.
     INITIALIZE                       JYR-REC.
*各量販店領域転送
*    取引先ＣＤ
     MOVE     WK-DEPB06      TO       JYR-F00.
*    受信日
     MOVE     ZERO          TO        JYR-F01.
*    発注日
     MOVE     WK-DEPB07     TO        JYR-F02.
*    納品日
     MOVE     WK-DEPB08     TO        JYR-F03.
*    店舗コード
     MOVE     WK-DEPB02     TO        JYR-F04.
*    店舗名
     MOVE     WK-DEPB12     TO        JYR-F05.
*    伝票番号
     MOVE     WK-DEPB05     TO        JYR-F06.
*    行番号
     MOVE     WK-DEPD03     TO        JYR-F07.
*    伝票区分
     MOVE     WK-DEPB04     TO        JYR-F08.
*    存在チェック
     READ  ROYJYRL1
           INVALID      MOVE  "INV"  TO  ROYJYRL1-INV-FLG
           NOT  INVALID MOVE  SPACE  TO  ROYJYRL1-INV-FLG
     END-READ.
*    判定
     IF    ROYJYRL1-INV-FLG = SPACE
           GO                 TO      EDIT-EXIT
     END-IF.
*    商品ＣＤ
     MOVE     ZERO          TO        JYR-F09.
*    商品名１
     MOVE     WK-DEPD16(1:15) TO      JYR-F101.
*    商品名２
     MOVE     WK-DEPD16(16:15) TO     JYR-F102.
*    発注数
     MOVE     WK-DEPD08     TO        JYR-F11.
*    検収数
     MOVE     WK-DEPD13     TO        JYR-F12.
*    原価単価
     MOVE     WK-DEPD09     TO        JYR-F13.
*    原価金額
     COMPUTE  JYR-F14 = WK-DEPD13 * WK-DEPD09.
*    ＪＡＮＣＤ
     MOVE     WK-DEPD05     TO        JYR-F15.
*    符号
     IF       WK-DEPB04  =  "13"
              MOVE   "1"    TO        JYR-F16(1:1)
     END-IF.
*    年月度
     MOVE   WK-DEPB11(1:6)  TO        JYR-F17.
*
     WRITE    JYR-REC.
     ADD      1             TO   WRT-CNT.
*
 EDIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      WRT-CNT   TO      OUT-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     CLOSE     ROYALJYR  ROYJYRL1.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
