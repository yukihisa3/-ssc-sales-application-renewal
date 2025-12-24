****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ベンダーオンライン　　　　　　　　*
*    モジュール名　　　　：　受領データ変換処理　　　　　　　　*
*    作成日／更新日　　　：　14/07/02                          *
*    作成者／更新者　　　：　NAV TAKAHASHI                     *
*    処理概要　　　　　　：　（ＣＶＣＳ）オンラインデータから　*
*                            受領累積データファイルを作成する　*
*                            グッディー用（九州支店）          *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NJH5751B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          14/07/02.
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
     SELECT   CVCSG001  ASSIGN    TO        DA-01-S-CVCSG001
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    DEN-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*当日スケジュールマスタ
     SELECT   JSMDAYL1  ASSIGN    TO        DA-01-VI-JSMDAYL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TJS-F01  TJS-F02
                                            TJS-F03
                        FILE  STATUS   IS   TJS-STATUS.
*グッデイ受領累積ファイル
     SELECT   GDJYURL1  ASSIGN    TO        DA-01-VI-GDJYURL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JOH-F01  JOH-F02
                                            JOH-F03  JOH-F04
                                            JOH-F05  JOH-F06
                                            JOH-F07  JOH-F08
                        FILE  STATUS   IS   JOH-STATUS.
*グッデイ出荷実績ファイル
     SELECT   GDSYUKL3  ASSIGN    TO        DA-01-VI-GDSYUKL3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SYK-A03  SYK-A02
                                            SYK-B02  SYK-B04
                        FILE  STATUS   IS   SYK-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    受信データ　ＲＬ＝　２５６　  ＢＦ＝　１
******************************************************************
 FD  CVCSG001
                        BLOCK CONTAINS      1    RECORDS
                        LABEL RECORD   IS   STANDARD.
*
 01  DEN-REC.
     03  DEN-01.
         05  DEN-01A             PIC  X(01).
         05  DEN-01C             PIC  X(255).
******************************************************************
*    当日スケジュールマスタ
******************************************************************
 FD  JSMDAYL1           LABEL RECORD   IS   STANDARD.
     COPY     JSMDAYF   OF        XFDLIB
              JOINING   TJS       PREFIX.
******************************************************************
*    グッデイ基本情報ファイル
******************************************************************
 FD  GDJYURL1           LABEL RECORD   IS   STANDARD.
     COPY     GDJYURL1  OF        XFDLIB
              JOINING   JOH       PREFIX.
*
******************************************************************
*    グッデイ出荷実績ファイル
******************************************************************
 FD  GDSYUKL3           LABEL RECORD   IS   STANDARD.
     COPY     GDSYUKL3  OF        XFDLIB
              JOINING   SYK       PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  IDX                     PIC  9(02)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT1                PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT2                PIC  9(08)     VALUE  ZERO.
 01  CNT-KENSU               PIC  9(08)     VALUE  ZERO.
 01  CNT-KENSU-D             PIC  9(08)     VALUE  ZERO.
 01  CNT-MAISU               PIC  9(08)     VALUE  ZERO.
 01  INV-RUT                 PIC  9(01)     VALUE  ZERO.
 01  FLG-TOK                 PIC  9(01)     VALUE  ZERO.
 01  WK-TOKCD                PIC  9(06)     VALUE  ZERO.
 01  GDJYURF-INV-FLG         PIC  X(03)     VALUE  ZERO.
 01  GDSYUKL3-INV-FLG        PIC  X(03)     VALUE  ZERO.
 01  FURIWAKE-CD             PIC  9(01)     VALUE  ZERO.
 01  DAIHYO-BASYO-CD         PIC  X(02)     VALUE  SPACE.
 01  SYUKA-BASYO             PIC  X(02)     VALUE  SPACE.
 01  WK-RUTO-CD              PIC  X(02)     VALUE  SPACE.
 01  SKIP-FLG                PIC  X(04)     VALUE  SPACE.
*ルート変換
 01  HEN-RUT.
     03  HEN-RUT-1           PIC  X(01)     VALUE  "0".
     03  HEN-RUT-2           PIC  X(01)     VALUE  SPACE.
*
*グループヘッドレコード退避ワーク
 01  WK-DEPA-REC.
     03  WK-DEPA01          PIC  X(256).
*ヘッドレコード退避ワーク
 01  WK-DEPB-REC.
     03  WK-DEPB01          PIC  X(01).
     03  WK-DEPB02          PIC  9(09).
     03  WK-DEPB03          PIC  9(03).
     03  WK-DEPB04          PIC  9(02).
     03  WK-DEPB05          PIC  X(01).
     03  WK-DEPB06          PIC  9(08).
     03  WK-DEPB07          PIC  9(08).
     03  WK-DEPB08          PIC  9(08).
     03  WK-DEPB09          PIC  9(06).
     03  WK-DEPB10          PIC  X(01).
     03  WK-DEPB11          PIC  N(20).
     03  WK-DEPB12          PIC  N(30).
     03  WK-DEPB13          PIC  N(30).
     03  WK-DEPB14          PIC  9(09).
     03  WK-DEPB15          PIC  9(01).
     03  WK-DEPB16          PIC  9(03).
     03  WK-DEPB17          PIC  X(36).
*    明細レコード退避ワーク
 01  WK-DEPD-REC.
     03  WK-DEPD01          PIC  X(01).
     03  WK-DEPD02          PIC  9(02).
     03  WK-DEPD03          PIC  9(07).
     03  WK-DEPD04          PIC  X(13).
     03  WK-DEPD05          PIC  X(01).
     03  WK-DEPD06          PIC  9(05).
     03  WK-DEPD07          PIC  9(08)V9(02).
     03  WK-DEPD08          PIC  9(08).
     03  WK-DEPD09          PIC  X(01).
     03  WK-DEPD10          PIC  9(12).
     03  WK-DEPD11          PIC  X(01).
     03  WK-DEPD12          PIC  9(12).
     03  WK-DEPD13          PIC  N(30).
     03  WK-DEPD14          PIC  N(30).
     03  WK-DEPD15          PIC  X(02).
     03  WK-DEPD16          PIC  X(61).
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  TJS-STATUS        PIC  X(02).
     03  JOH-STATUS        PIC  X(02).
     03  SYK-STATUS        PIC  X(02).
*取引先ＣＤ変換（文字⇒数値）
 01  WK-TORICD             PIC  X(06).
 01  WK-TORICD-R           REDEFINES  WK-TORICD.
     03  WK-HEN-TORICD     PIC  9(06).
*伝票番号ＣＤ変換（文字⇒数値）
 01  WK-DENNO              PIC  X(08).
 01  WK-DENNO-R            REDEFINES  WK-DENNO.
     03  WK-HEN-DENNO      PIC  9(08).
*原価単価（数値⇒数値）
 01  WK-GENKA              PIC  9(10).
 01  WK-GENKA-R            REDEFINES  WK-GENKA.
     03  WK-HEN-GENKA      PIC  9(08)V9(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NJH5751B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NJH5751B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NJH5751B".
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
     03  MSG-JOH.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " JOHWRT= ".
         05  JOH-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-ERR.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " JOHERR= ".
         05  ERR-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-AREA.
     03  PARA-JDATE         PIC   9(08).
     03  PARA-JTIME         PIC   9(04).
     03  PARA-KSYU          PIC   X(01).
     03  PARA-YUSEN         PIC   X(01).
     03  PARA-MAISU         PIC   9(08).
     03  PARA-KENSU         PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-AREA.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   CVCSG001.
     MOVE      "CVCSG001"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JSMDAYL1.
     MOVE      "JSMDAYL1"   TO   AB-FILE.
     MOVE      TJS-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   GDJYURL1.
     MOVE      "GDJYURL1"   TO   AB-FILE.
     MOVE      JOH-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   GDSYUKL3.
     MOVE      "GDSYUKL3"   TO   AB-FILE.
     MOVE      SYK-STATUS   TO   AB-STS.
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
     OPEN     INPUT     CVCSG001  GDSYUKL3.
     OPEN     I-O       JSMDAYL1  GDJYURL1.
     DISPLAY  MSG-START UPON CONS.
*
*    MOVE     PARA-MAISU TO       CNT-MAISU.
*    MOVE     PARA-KENSU TO       CNT-KENSU.
     MOVE     ZERO       TO       CNT-MAISU.
     MOVE     ZERO       TO       CNT-KENSU.
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
     READ     CVCSG001
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
*ファイルヘッダ処理
     IF    DEN-01A  =  "A"
           MOVE      DEN-01      TO   WK-DEPA-REC
     END-IF.
*伝票ヘッダ処理
     IF    DEN-01A  =  "B"
             MOVE      SPACE       TO   WK-DEPB-REC
             INITIALIZE                 WK-DEPB-REC
             MOVE      DEN-01      TO   WK-DEPB-REC
             ADD       1           TO   CNT-MAISU
             MOVE      ZERO        TO   CNT-KENSU-D
     END-IF.
*明細行
     IF    DEN-01A  =  "D"
           MOVE      DEN-01      TO   WK-DEPD-REC
           ADD       1           TO   CNT-KENSU
           ADD       1           TO   CNT-KENSU-D
           PERFORM   EDIT-SEC
     END-IF.
*
 MAIN-99.
     READ     CVCSG001
              AT END    MOVE      9         TO  END-FG
                        GO                  TO  MAIN-EXIT
              NOT AT END
                        ADD       1         TO  RD-CNT
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
*
     MOVE     SPACE         TO        JOH-REC.
     INITIALIZE                       JOH-REC.
*グッデイ受領累積ファイル作成
*キー部分セット
*    受信日
     MOVE     PARA-JDATE    TO        JOH-F01.
*    受信時刻
     MOVE     PARA-JTIME    TO        JOH-F02.
*    受信取引先ＣＤ
     MOVE     WK-DEPB09     TO        JOH-F03  WK-TOKCD.
*    検品日／返品日
     MOVE     WK-DEPB06     TO        JOH-F04.
*    店舗ＣＤ
     MOVE     WK-DEPB03     TO        JOH-F05.
*    伝票区分
     MOVE     WK-DEPB04     TO        JOH-F06.
*    伝票番号
     MOVE     WK-DEPB02     TO        JOH-F07.
*    行番号
     MOVE     WK-DEPD02     TO        JOH-F08.
*    ヘッダ行セット
     MOVE     WK-DEPB-REC   TO        JOH-A00.
*    明細行セット
     MOVE     WK-DEPD-REC   TO        JOH-B00.
*出荷実績チェック
     MOVE     JOH-A03       TO        SYK-A03.
     MOVE     JOH-A02       TO        SYK-A02.
     MOVE     JOH-B02       TO        SYK-B02.
     MOVE     JOH-B04       TO        SYK-B04.
     PERFORM  GDSYUKL3-READ-SEC.
     IF       GDSYUKL3-INV-FLG = "INV"
              MOVE "1"      TO        JOH-F99
     ELSE
              IF  JOH-B06 NOT = SYK-F09
                  MOVE  "2" TO        JOH-F99
              END-IF
              MOVE SYK-F09  TO        JOH-F98
              MOVE SYK-F08  TO        JOH-F97
     END-IF.
*存在チェック
     PERFORM GDJYURF-READ-SEC.
     IF  GDJYURF-INV-FLG = "INV"
         WRITE  JOH-REC
         ADD    1           TO   WRT-CNT1
     END-IF.
*
 EDIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
     IF        CNT-MAISU     >    ZERO
*              当日スケジュールマスタ出力
               PERFORM   JSMDAYL1-WRT-SEC
     END-IF.
*
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      WRT-CNT1  TO      JOH-CNT.
     MOVE      WRT-CNT2  TO      ERR-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-JOH   UPON CONS.
*
     CLOSE     CVCSG001  GDSYUKL3
               JSMDAYL1  GDJYURL1.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　　当日スケジュールマスタ出力　　　　　　　　　　　*
****************************************************************
 JSMDAYL1-WRT-SEC        SECTION.
*
     MOVE   "JSMDAYL1-WRT-SEC"  TO   S-NAME.
     MOVE    SPACE         TO        TJS-REC.
     INITIALIZE                      TJS-REC.
     MOVE    PARA-JDATE    TO        TJS-F01.
     MOVE    PARA-JTIME    TO        TJS-F02.
     MOVE    WK-TOKCD      TO        TJS-F03.
     READ    JSMDAYL1
       INVALID
         CONTINUE
       NOT INVALID
         GO  TO   JSMDAYL1-010
     END-READ.
*
     MOVE    SPACE         TO        TJS-REC.
     INITIALIZE                      TJS-REC.
     MOVE    PARA-JDATE    TO        TJS-F01.
     MOVE    PARA-JTIME    TO        TJS-F02.
     MOVE    WK-TOKCD      TO        TJS-F03.
     MOVE    02            TO        TJS-F04.
     MOVE    CNT-KENSU     TO        TJS-F09.
     MOVE    CNT-MAISU     TO        TJS-F10.
     WRITE   TJS-REC.
     GO      TO    JSMDAYL1-WRT-EXIT.
*
 JSMDAYL1-010.
*
     MOVE    CNT-KENSU     TO        TJS-F09.
     MOVE    CNT-MAISU     TO        TJS-F10.
     REWRITE TJS-REC.
*
 JSMDAYL1-WRT-EXIT.
     EXIT.
****************************************************************
*　　グッデイ受領累積ファイル索引
****************************************************************
 GDJYURF-READ-SEC          SECTION.
*
     READ     GDJYURL1
         INVALID
           MOVE  "INV"     TO        GDJYURF-INV-FLG
         NOT INVALID
           MOVE  SPACE     TO        GDJYURF-INV-FLG
     END-READ.
*
 GDJYURF-READ-EXIT.
     EXIT.
****************************************************************
*　　グッデイ受領累積ファイル索引
****************************************************************
 GDSYUKL3-READ-SEC         SECTION.
*
     READ     GDSYUKL3
         INVALID
           MOVE  "INV"     TO        GDSYUKL3-INV-FLG
         NOT INVALID
           MOVE  SPACE     TO        GDSYUKL3-INV-FLG
     END-READ.
*
 GDSYUKL3-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
