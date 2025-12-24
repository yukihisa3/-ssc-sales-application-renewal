# NKE0220B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NKE0220B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　入荷検品　　　                    *
*    モジュール名　　　　：　入荷予定データ抽出                *
*    　　　　　　　　　　　　（社内発注分）　　                *
*    作成日／作成者　　　：　2019/01/21 INOUE                  *
*    処理概要　　　　　　：　検品システムに引き渡す入荷予定　  *
*      　　　　　　　　　　　データを抽出する。　　　　　　　　*
*　　更新日／更新者　　　：　                                  *
*    修正概要　　　　　　：　　　　　　　　　　　　　　        *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NKE0220B.
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
*社内振替情報ファイル
     SELECT   SFRHEDL2  ASSIGN    TO        DA-01-VI-SFRHEDL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       HED-F04  HED-F14
                                            HED-F02  HED-F03
                        FILE  STATUS   IS   HED-STATUS.
*発注明細ファイル
*    SELECT   HACMEIL1  ASSIGN    TO        DA-01-VI-HACMEIL1
*                       ORGANIZATION        INDEXED
*                       ACCESS    MODE      SEQUENTIAL
*                       RECORD    KEY       HED-F02
*                                           HED-F03
*                       FILE  STATUS   IS   HED-STATUS.
*商品名称マスタ
*    SELECT     MEIMS1  ASSIGN    TO        DA-01-VI-MEIMS1
*                       ORGANIZATION        INDEXED
*                       ACCESS    MODE      RANDOM
*                       RECORD    KEY       MEI-F011  MEI-F0121
*                                           MEI-F0122 MEI-F0123
*                       FILE      STATUS    MEI-STATUS.
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
*    社内振替情報ファイル
******************************************************************
 FD  SFRHEDL2
                        LABEL RECORD   IS   STANDARD.
     COPY     SFRHEDL2  OF        XFDLIB
              JOINING   HED  AS   PREFIX.
*
******************************************************************
*    発注明細ファイル
******************************************************************
*FD  HACMEIL1
*                       LABEL RECORD   IS   STANDARD.
*    COPY     HACMEIL1  OF        XFDLIB
*             JOINING   HME  AS   PREFIX.
*
******************************************************************
*    商品名称マスタ
******************************************************************
*FD  MEIMS1             LABEL RECORD   IS   STANDARD.
*    COPY     MEIMS1    OF        XFDLIB
*    JOINING  MEI       AS        PREFIX.
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
     03  HED-STATUS        PIC  X(02).
     03  HME-STATUS        PIC  X(02).
     03  NYK-STATUS        PIC  X(02).
     03  NKK-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NKE0220B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NKE0220B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NKE0220B".
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
*FILEERR-SEC1           SECTION.
*    USE       AFTER    EXCEPTION
*                       PROCEDURE   HACMEIL1.
*    MOVE      "HACMEIL1"   TO   AB-FILE.
*    MOVE      HED-STATUS   TO   AB-STS.
*    DISPLAY   MSG-ABEND         UPON CONS.
*    DISPLAY   SEC-NAME          UPON CONS.
*    DISPLAY   ABEND-FILE        UPON CONS.
*    MOVE      4000         TO   PROGRAM-STATUS.
*    STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SFRHEDL2.
     MOVE      "SFRHEDL2"   TO   AB-FILE.
     MOVE      HED-STATUS   TO   AB-STS.
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
*FILEERR-SEC5           SECTION.
*    USE       AFTER    EXCEPTION
*                       PROCEDURE   MEIMS1.
*    MOVE      "MEIMS1   "  TO   AB-FILE.
*    MOVE      MEI-STATUS   TO   AB-STS.
*    DISPLAY   MSG-ABEND         UPON CONS.
*    DISPLAY   SEC-NAME          UPON CONS.
*    DISPLAY   ABEND-FILE        UPON CONS.
*    MOVE      4000         TO   PROGRAM-STATUS.
*    STOP      RUN.
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
     OPEN     INPUT     SFRHEDL2.
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
     MOVE     SPACE           TO   HED-REC.
     INITIALIZE                    HED-REC.
     MOVE     PARA-IN-SOKCD   TO   HED-F04.
     MOVE     PARA-IN-FDATE   TO   HED-F14.
     MOVE     ZERO            TO   HED-F02.
     MOVE     SPACE           TO   HED-F03.
     START    SFRHEDL2  KEY   >=   HED-F04 HED-F14 HED-F02 HED-F03
         INVALID   KEY
              MOVE      9    TO   END-FG
              GO   TO   INIT-EXIT
     END-START.
*
 INIT-010.
*
     READ     SFRHEDL2
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
*    倉庫チェック
     IF       PARA-IN-SOKCD     =    HED-F04
              CONTINUE
     ELSE
              MOVE      9         TO   END-FG
              GO        TO        MAIN-EXIT
     END-IF.
*    入力日・完納区分・発注数チェック
     IF     ( PARA-IN-FDATE    <=    HED-F14 ) AND
            ( PARA-IN-TDATE    >=    HED-F14 ) AND
            ( HED-F24           =    " "     ) AND
            ( HED-F15       NOT =    ZERO    )
              CONTINUE
     ELSE
              GO        TO        MAIN-010
     END-IF.
*    発注残数チェック
     COMPUTE  CAL-ZANSU = HED-F15 - HED-F19.
     IF       CAL-ZANSU         >    0
              CONTINUE
     ELSE
              GO        TO        MAIN-010
     END-IF.
*
*MAIN-002.
*    明細START
*    MOVE     SPACE           TO   HED-REC.
*    INITIALIZE                    HED-REC.
*    MOVE     HED-F02         TO   HED-F02.
*    START    HACMEIL1  KEY   >=   HED-F02
*        INVALID   KEY
*             GO   TO   MAIN-010
*    END-START.
*
*MAIN-003.
*    明細READ
*    READ     HACMEIL1
*             AT END    GO   TO   MAIN-010
*             NOT AT END
*                       ADD       1         TO  HM-RD-CNT
*    END-READ.
*    明細対象チェック(伝票番号)
*    IF       HED-F02          =    HED-F02
*             CONTINUE
*    ELSE
*             GO   TO   MAIN-010
*    END-IF.
*    明細対象チェック(完了区分・入荷残数)
*    COMPUTE  CAL-ZANSU = HED-F09 - HED-F10.
*    IF     ( HED-F05          =    0 ) AND
*           ( CAL-ZANSU        >    0 )
*             CONTINUE
*    ELSE
*             GO   TO   MAIN-003
*    END-IF.
*
*    明細対象チェック(名称マスタあり)
*    MOVE     HED-F06             TO   MEI-F011.
*    MOVE     HED-F07(1:5)        TO   MEI-F0121.
*    MOVE     HED-F07(6:2)        TO   MEI-F0122.
*    MOVE     HED-F07(8:1)        TO   MEI-F0123.
*    READ     MEIMS1
*             INVALID
*                       DISPLAY NC"名称マスタなし！" UPON CONS
*                       DISPLAY NC"商品ＣＤ＝" HED-F06 UPON CONS
*                       DISPLAY NC"品単ＣＤ＝" HED-F07 UPON CONS
*                       ADD     1    TO   ER-CNT
*                       GO      TO   MAIN-003
*             NOT INVALID
*                       ADD       1         TO  HM-RD-CNT
*    END-READ.
*    明細対象チェック(名称マスタＪＡＮ登録あり)
*    IF       MEI-F06      NOT =    SPACE
*             CONTINUE
*    ELSE
*             DISPLAY NC"名称マスタにＪＡＮＣＤなし！" UPON CONS
*             DISPLAY NC"商品ＣＤ＝" HED-F06 UPON CONS
*             DISPLAY NC"品単ＣＤ＝" HED-F07 UPON CONS
*             ADD     1    TO   ER-CNT
*             GO      TO   MAIN-003
*    END-IF.
*
*
 MAIN-004.
*入荷予定ファイル出力
     MOVE     SPACE          TO   NYK-REC.
     INITIALIZE                   NYK-REC.
*
*   発注伝票_
     MOVE     HED-F28        TO   NYK-F01.
*   発注行_
     MOVE     1              TO   NYK-F02.
*   仕入先ＣＤ
     MOVE     HED-F12        TO   NYK-F03.
*   入荷予定日
     MOVE     HED-F14(1:4)   TO   NYK-F04(1:4).
     MOVE     "/"            TO   NYK-F04(5:1).
     MOVE     HED-F14(5:2)   TO   NYK-F04(6:2).
     MOVE     "/"            TO   NYK-F04(8:1).
     MOVE     HED-F14(7:2)   TO   NYK-F04(9:2).
*   ＪＡＮＣＤ
     MOVE     HED-F13        TO   NYK-F05.
*   サカタ商品CD + 品単CD
     MOVE     HED-F05        TO   NYK-F06(1:8).
     MOVE     HED-F06        TO   NYK-F06(9:5).
     MOVE     HED-F07        TO   NYK-F06(14:2).
     MOVE     HED-F08        TO   NYK-F06(16:1).
*   発注区分
     MOVE     "2"            TO   NYK-F07.
*   棚番
     MOVE     HED-F11        TO   NYK-F08.
*
*   発注数
     MOVE     HED-F15        TO   NYK-F09.
*
*   発注残数
*    COMPUTE  CAL-ZANSU = HED-F15 - HED-F19.
     MOVE     CAL-ZANSU      TO   NYK-F10.
*
     WRITE    NYK-REC.
     ADD      1              TO   WRT-CNT
                                  CAL-KENSU.
*
 MAIN-010.
*
     READ     SFRHEDL2
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
*    MOVE      HM-RD-CNT TO      HM-IN-CNT.
*    MOVE      ER-CNT    TO      ERR-CNT.
     MOVE      WRT-CNT   TO      OUT-CNT.
     DISPLAY   MSG-HD-IN UPON CONS.
*    DISPLAY   MSG-HM-IN UPON CONS.
*    DISPLAY   MSG-ERR   UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     CLOSE     SFRHEDL2  SNDNYKXX SNDNKKXX.
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
