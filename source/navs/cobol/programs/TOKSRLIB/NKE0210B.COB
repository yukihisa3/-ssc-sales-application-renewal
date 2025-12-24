****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　入荷検品　　　                    *
*    モジュール名　　　　：　入荷予定データ抽出                *
*    　　　　　　　　　　　　（メーカー発注分）                *
*    作成日／作成者　　　：　2019/01/21 INOUE                  *
*    処理概要　　　　　　：　検品システムに引き渡す入荷予定　  *
*      　　　　　　　　　　　データを抽出する。　　　　　　　　*
*　　更新日／更新者　　　：　                                  *
*    修正概要　　　　　　：　　　　　　　　　　　　　　        *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NKE0210B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2019/01/21.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*発注ヘッダファイル
     SELECT   HACHEDL2  ASSIGN    TO        DA-01-VI-HACHEDL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       HHD-F17
                                            HHD-F02
                        FILE  STATUS   IS   HHD-STATUS.
*発注明細ファイル
     SELECT   HACMEIL1  ASSIGN    TO        DA-01-VI-HACMEIL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       HME-F02
                                            HME-F03
                        FILE  STATUS   IS   HME-STATUS.
*商品名称マスタ
     SELECT     MEIMS1  ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F011  MEI-F0121
                                            MEI-F0122 MEI-F0123
                        FILE      STATUS    MEI-STATUS.
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
*    発注ヘッダファイル
******************************************************************
 FD  HACHEDL2
                        LABEL RECORD   IS   STANDARD.
     COPY     HACHEDL2  OF        XFDLIB
              JOINING   HHD  AS   PREFIX.
*
******************************************************************
*    発注明細ファイル
******************************************************************
 FD  HACMEIL1
                        LABEL RECORD   IS   STANDARD.
     COPY     HACMEIL1  OF        XFDLIB
              JOINING   HME  AS   PREFIX.
*
******************************************************************
*    商品名称マスタ
******************************************************************
 FD  MEIMS1             LABEL RECORD   IS   STANDARD.
     COPY     MEIMS1    OF        XFDLIB
     JOINING  MEI       AS        PREFIX.
******************************************************************
*    入荷予定ファイル
******************************************************************
 FD  SNDNYKXX           BLOCK     CONTAINS  48   RECORDS.
     COPY     SNDNYKXX  OF        XFDLIB
              JOINING   NYK       PREFIX.
******************************************************************
*    送信件数ファイル
******************************************************************
 FD  SNDNKKXX            BLOCK CONTAINS 1    RECORDS
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
 01  WK-ST.
     03  HME-STATUS        PIC  X(02).
     03  HHD-STATUS        PIC  X(02).
     03  NYK-STATUS        PIC  X(02).
     03  NKK-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NKE0210B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NKE0210B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NKE0210B".
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
         05  FILLER         PIC   X(12)  VALUE " INPUT HED= ".
         05  HD-IN-CNT      PIC   9(07).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-HM-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(12)  VALUE " INPUT MEI= ".
         05  HM-IN-CNT      PIC   9(07).
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
 01  PARA-IN-KENSU          PIC   9(07).
 01  PARA-IN-BUMCD          PIC   X(04).
 01  PARA-IN-TANCD          PIC   X(02).
 01  PARA-IN-SOKCD          PIC   X(02).
 01  PARA-IN-FDATE          PIC   9(08).
 01  PARA-IN-TDATE          PIC   9(08).
 01  PARA-OUT-KENSU         PIC   9(07).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-IN-KENSU
                                       PARA-IN-BUMCD
                                       PARA-IN-TANCD
                                       PARA-IN-SOKCD
                                       PARA-IN-FDATE
                                       PARA-IN-TDATE
                                       PARA-OUT-KENSU.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HACMEIL1.
     MOVE      "HACMEIL1"   TO   AB-FILE.
     MOVE      HME-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HACHEDL2.
     MOVE      "HACHEDL2"   TO   AB-FILE.
     MOVE      HHD-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
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
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   MEIMS1.
     MOVE      "MEIMS1   "  TO   AB-FILE.
     MOVE      MEI-STATUS   TO   AB-STS.
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
     OPEN     INPUT     HACMEIL1  HACHEDL2  MEIMS1.
     OPEN     EXTEND    SNDNYKXX.
     OPEN     OUTPUT    SNDNKKXX.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG.
     MOVE     ZERO      TO        HD-RD-CNT HD-IN-CNT
                                  HM-RD-CNT HM-IN-CNT
                                  ER-CNT    ERR-CNT
                                  WRT-CNT   OUT-CNT.
*
     MOVE     PARA-IN-KENSU   TO   CAL-KENSU.
*
     MOVE     SPACE           TO   HHD-REC.
     INITIALIZE                    HHD-REC.
     MOVE     PARA-IN-SOKCD   TO   HHD-F17.
     MOVE     ZERO            TO   HHD-F02.
     START    HACHEDL2  KEY   >=   HHD-F17   HHD-F02
         INVALID   KEY
              MOVE      9    TO   END-FG
              GO   TO   INIT-EXIT
     END-START.
*
 INIT-010.
*
     READ     HACHEDL2
              AT END    MOVE      9         TO  END-FG
              NOT AT END
                        ADD       1         TO  HD-RD-CNT
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
*    倉庫・入力日チェック
     IF       PARA-IN-SOKCD     =    HHD-F17
              CONTINUE
     ELSE
              MOVE      9         TO   END-FG
              GO        TO        MAIN-EXIT
     END-IF.
*
     IF     ( PARA-IN-FDATE    <=    HHD-F11 ) AND
            ( PARA-IN-TDATE    >=    HHD-F11 ) AND
            ( HHD-F01           =    50      ) AND
            ( HHD-F04           =    0       )
              CONTINUE
     ELSE
              GO        TO        MAIN-010
     END-IF.
*
 MAIN-002.
*    明細START
     MOVE     SPACE           TO   HME-REC.
     INITIALIZE                    HME-REC.
     MOVE     HHD-F02         TO   HME-F02.
     START    HACMEIL1  KEY   >=   HME-F02
         INVALID   KEY
              GO   TO   MAIN-010
     END-START.
*
 MAIN-003.
*    明細READ
     READ     HACMEIL1
              AT END    GO   TO   MAIN-010
              NOT AT END
                        ADD       1         TO  HM-RD-CNT
     END-READ.
*    明細対象チェック(伝票番号)
     IF       HME-F02          =    HHD-F02
              CONTINUE
     ELSE
              GO   TO   MAIN-010
     END-IF.
*    明細対象チェック(完了区分・入荷残数)
     COMPUTE  CAL-ZANSU = HME-F09 - HME-F10.
     IF     ( HME-F05          =    0 ) AND
            ( CAL-ZANSU        >    0 )
              CONTINUE
     ELSE
              GO   TO   MAIN-003
     END-IF.
*
*    明細対象チェック(名称マスタあり)
     MOVE     HME-F06             TO   MEI-F011.
     MOVE     HME-F07(1:5)        TO   MEI-F0121.
     MOVE     HME-F07(6:2)        TO   MEI-F0122.
     MOVE     HME-F07(8:1)        TO   MEI-F0123.
     READ     MEIMS1
              INVALID
                        DISPLAY NC"名称マスタなし！" UPON CONS
                        DISPLAY NC"商品ＣＤ＝" HME-F06 UPON CONS
                        DISPLAY NC"品単ＣＤ＝" HME-F07 UPON CONS
                        ADD     1    TO   ER-CNT
                        GO      TO   MAIN-003
              NOT INVALID
                        ADD       1         TO  HM-RD-CNT
     END-READ.
*    明細対象チェック(名称マスタＪＡＮ登録あり)
     IF       MEI-F06      NOT =    SPACE
              CONTINUE
     ELSE
              DISPLAY NC"名称マスタにＪＡＮＣＤなし！" UPON CONS
              DISPLAY NC"商品ＣＤ＝" HME-F06 UPON CONS
              DISPLAY NC"品単ＣＤ＝" HME-F07 UPON CONS
              ADD     1    TO   ER-CNT
              GO      TO   MAIN-003
     END-IF.
*
*MAIN-004.
*    振分情報ファイル検索
*    MOVE     MEI-F01        TO   HHD-F17.
*    MOVE     MEI-F02        TO   HHD-F02.
*    MOVE     MEI-F03        TO   HHD-F03.
*    MOVE     MEI-F02        TO   HHD-F04.
*    MOVE     MEI-F05        TO   HHD-F05.
*    MOVE     MEI-F06        TO   HHD-F06.
*    MOVE     MEI-F07        TO   HHD-F07.
*    MOVE     MEI-F08        TO   HHD-F08.
*    READ     HACHEDL2
*         INVALID
*             DISPLAY NC"振分情報ファイルなし！" UPON CONS
*             DISPLAY NC"伝票区分　＝" MEI-F01   UPON CONS
*             DISPLAY NC"年度　　　＝" MEI-F02   UPON CONS
*             DISPLAY NC"シーズン　＝" MEI-F03   UPON CONS
*             DISPLAY NC"倉庫ＣＤ　＝" MEI-F02   UPON CONS
*             DISPLAY NC"商品ＣＤ　＝" MEI-F05   UPON CONS
*             DISPLAY NC"品単１　　＝" MEI-F06   UPON CONS
*             DISPLAY NC"品単２　　＝" MEI-F07   UPON CONS
*             DISPLAY NC"品単３　　＝" MEI-F08   UPON CONS
*             MOVE    4010    TO   PROGRAM-STATUS
*             STOP    RUN
*    END-READ.
*
*MAIN-005.
*
*    完納区分チェック
*    IF       PARA-IN-KANKBN   =    SPACE
*             CONTINUE
*    ELSE
*             IF   PARA-IN-KANKBN   =    "1"
*                  IF   HHD-F24          =    " "
*                       CONTINUE
*                  ELSE
*                       GO        TO        MAIN-010
*                  END-IF
*             END-IF
*             IF   PARA-IN-KANKBN   =    "2"
*                  IF   HHD-F24          =    "1"
*                       CONTINUE
*                  ELSE
*                       GO        TO        MAIN-010
*                  END-IF
*             END-IF
*    END-IF.
*
 MAIN-004.
*入荷予定ファイル出力
     MOVE     SPACE          TO   NYK-REC.
     INITIALIZE                   NYK-REC.
*
*   発注伝票_
     MOVE     HME-F02        TO   NYK-F01.
*   発注行_
     MOVE     HME-F03        TO   NYK-F02.
*   仕入先ＣＤ
     MOVE     HHD-F06        TO   NYK-F03.
*   入荷予定日
     MOVE     HHD-F11(1:4)   TO   NYK-F04(1:4).
     MOVE     "/"            TO   NYK-F04(5:1).
     MOVE     HHD-F11(5:2)   TO   NYK-F04(6:2).
     MOVE     "/"            TO   NYK-F04(8:1).
     MOVE     HHD-F11(7:2)   TO   NYK-F04(9:2).
*   ＪＡＮＣＤ
     MOVE     MEI-F06        TO   NYK-F05.
*   サカタ商品CD + 品単CD
     MOVE     HME-F06        TO   NYK-F06(1:8).
     MOVE     HME-F07        TO   NYK-F06(9:8).
*   発注区分
     MOVE     "1"            TO   NYK-F07.
*   棚番
     MOVE     HME-F08        TO   NYK-F08.
*
*   発注数
     MOVE     HME-F09        TO   NYK-F09.
*
*   発注残数
     MOVE     CAL-ZANSU      TO   NYK-F10.
*
     WRITE    NYK-REC.
     ADD      1              TO   WRT-CNT
                                  CAL-KENSU.
     GO                      TO   MAIN-003.
*
 MAIN-010.
*
     READ     HACHEDL2
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
     MOVE      CAL-KENSU TO      NKK-F01 PARA-OUT-KENSU.
     MOVE      X"0D0A"   TO      NKK-F02.
     WRITE                       NKK-REC.
*
     MOVE      HD-RD-CNT TO      HD-IN-CNT.
     MOVE      HM-RD-CNT TO      HM-IN-CNT.
*    MOVE      ER-CNT    TO      ERR-CNT.
     MOVE      WRT-CNT   TO      OUT-CNT.
     DISPLAY   MSG-HD-IN UPON CONS.
     DISPLAY   MSG-HM-IN UPON CONS.
*    DISPLAY   MSG-ERR   UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     CLOSE     HACMEIL1  HACHEDL2  SNDNYKXX SNDNKKXX MEIMS1.
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
