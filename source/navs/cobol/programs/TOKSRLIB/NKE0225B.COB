****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　入荷検品　　　                    *
*    モジュール名　　　　：　入荷予定データ重複排除            *
*    作成日／作成者　　　：　2019/02/04 INOUE                  *
*    処理概要　　　　　　：　同一発注伝票_・JANCDのレコードを
*      　　　　　　　　　　　排除する＆ＳＹＳＯＵＴ。　　　　　*
*      　　　　　　　　　　　あわせて件数も再セットする。　　　*
*　　更新日／更新者　　　：　                                  *
*    修正概要　　　　　　：　　　　　　　　　　　　　　        *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NKE0225B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2019/02/04.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*入荷予定ワーク
     SELECT   WRKNYKXX  ASSIGN    TO        DA-01-S-WRKNYKXX
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE  STATUS   IS   WRK-STATUS.
*入荷予定ファイル
     SELECT   SNDNYKXX  ASSIGN    TO        DA-01-S-SNDNYKXX
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   NYK-STATUS.
*送信用件数Ｆ
     SELECT   SNDNKKXX  ASSIGN              DA-01-S-SNDNKKXX
                        ORGANIZATION        SEQUENTIAL
                        STATUS              NKK-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    入荷予定ワーク
******************************************************************
 FD  WRKNYKXX           BLOCK     CONTAINS  48   RECORDS.
     COPY     SNDNYKXX  OF        XFDLIB
              JOINING   WRK  AS   PREFIX.
*
******************************************************************
*    入荷予定ファイル
******************************************************************
 FD  SNDNYKXX           BLOCK     CONTAINS  48   RECORDS.
     COPY     SNDNYKXX  OF        XFDLIB
              JOINING   NYK       PREFIX.
******************************************************************
*    送信件数ファイル
******************************************************************
 FD  SNDNKKXX           BLOCK CONTAINS  1   RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  NKK-REC.
     03  NKK-F01             PIC  9(08).
     03  NKK-F02             PIC  X(02).
*****************************************************************
 WORKING-STORAGE        SECTION.
*
*    COPY   NFSHIRED OF XFDLIB  JOINING   NFD  AS   PREFIX.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  HD-RD-CNT               PIC  9(08)     VALUE  ZERO.
 01  HM-RD-CNT               PIC  9(08)     VALUE  ZERO.
 01  ER-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  CAL-ZANSU               PIC S9(08)     VALUE  ZERO.
 01  CAL-KENSU               PIC  9(07)     VALUE  ZERO.
*
*ブレイク判定項目
 01  BRK-WRK-F01             PIC  9(07)     VALUE  ZERO.
 01  BRK-WRK-F05             PIC  X(13)     VALUE  SPACE.
*
 01  WK-ST.
     03  WRK-STATUS        PIC  X(02).
     03  NYK-STATUS        PIC  X(02).
     03  NKK-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NKE0225B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NKE0225B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NKE0225B".
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
     03  MSG-HD-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  HD-IN-CNT      PIC   9(07).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-ERR.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " ERROR = ".
         05  ERR-CNT        PIC   9(07).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT        PIC   9(07).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*------------------------------------------------------------
 LINKAGE                SECTION.
 01  PARA-OUT-KENSU         PIC   9(07).
 01  PARA-ERR-KENSU         PIC   9(07).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-OUT-KENSU
                                       PARA-ERR-KENSU.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   WRKNYKXX.
     MOVE      "WRKNYKXX"   TO   AB-FILE.
     MOVE      WRK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SNDNYKXX.
     MOVE      "SNDNYKXX "  TO   AB-FILE.
     MOVE      NYK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SNDNKKXX.
     MOVE      "SNDNKKXX"   TO   AB-FILE.
     MOVE      NKK-STATUS   TO   AB-STS.
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
     OPEN     INPUT     WRKNYKXX.
     OPEN     OUTPUT    SNDNYKXX.
     OPEN     OUTPUT    SNDNKKXX.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG.
     MOVE     ZERO      TO        HD-RD-CNT HD-IN-CNT
*                                 HM-RD-CNT HM-IN-CNT
                                  ER-CNT    ERR-CNT
                                  WRT-CNT   OUT-CNT.
*
*    MOVE     PARA-IN-KENSU   TO   CAL-KENSU.
*
 INIT-010.
*
     READ     WRKNYKXX
              AT END    MOVE      9         TO  END-FG
              NOT AT END
                        ADD       1         TO  HD-RD-CNT
*                       MOVE      WRK-F01   TO  BRK-WRK-F01
*                       MOVE      WRK-F05   TO  BRK-WRK-F05
     END-READ.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*
 MAIN-001.
*
*同一発注伝票_・ＪＡＮＣＤ別制御
*    同一の場合：除外対象
     IF     ( WRK-F01   =  BRK-WRK-F01 ) AND
            ( WRK-F05   =  BRK-WRK-F05 )
              IF  ERR-CNT   =  0
                  DISPLAY
                        NC"入荷予定データ"
                  DISPLAY
                        NC"　発注伝票_・ＪＡＮＣＤ　重複リスト"
                  DISPLAY
                        NC"　　　　　　　　　　　　　　　　　　"
              END-IF
              DISPLAY   NC"　重複：発注伝票_＝"   WRK-F01
                        NC"　行_＝"   WRK-F02
              ADD       1         TO   ERR-CNT
*    同一でない場合：出力対象
     ELSE
              MOVE      WRK-REC   TO   NYK-REC
              WRITE     NYK-REC
              ADD       1         TO   WRT-CNT
              MOVE      WRK-F01   TO   BRK-WRK-F01
              MOVE      WRK-F05   TO   BRK-WRK-F05
     END-IF.
*
 MAIN-010.
*
     READ     WRKNYKXX
              AT  END    MOVE      9         TO  END-FG
              NOT AT  END
                  ADD 1    TO   HD-RD-CNT
     END-READ.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
*件数ファイル出力
     MOVE      SPACE     TO      NKK-REC.
     INITIALIZE                  NKK-REC.
     MOVE      WRT-CNT   TO      NKK-F01 PARA-OUT-KENSU.
     MOVE      X"0D0A"   TO      NKK-F02.
     WRITE                       NKK-REC.
*
     MOVE      HD-RD-CNT TO      HD-IN-CNT.
*    MOVE      HM-RD-CNT TO      HM-IN-CNT.
     MOVE      ERR-CNT   TO      ERR-CNT  PARA-ERR-KENSU.
     MOVE      WRT-CNT   TO      OUT-CNT.
     DISPLAY   MSG-HD-IN UPON CONS.
*    DISPLAY   MSG-HM-IN UPON CONS.
     DISPLAY   MSG-ERR   UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     CLOSE     WRKNYKXX  SNDNYKXX SNDNKKXX.
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
