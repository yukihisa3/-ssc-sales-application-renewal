# SSK0035B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSK0035B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ケーヨー伝票レス                  *
*    モジュール名　　　　：　出荷実績データ抽出                *
*                           （過去分マッチング用）             *
*    作成日／作成者　　　：　2014/03/27  YOSHIDA.M             *
*    処理概要　　　　　　：　納品日範囲のパラメタを受取り、ケー*
*                            ヨー・パラメタ納品日開始で売上累積*
*                            データにスタートを掛け、パラメータ*
*                            納品日範囲のデータを出荷実績抽出  *
*                            データに出力する。                *
*                            出荷実績抽出データに同一キーが存在*
*                            する場合は、データ出力は行わない。*
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SSK0035B.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*売上累積データ
     SELECT   KEIURIL3  ASSIGN    TO        DA-01-VI-KEIURIL3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       URI-F01   URI-F112
                        FILE  STATUS   IS   URI-STATUS.
*出荷実績抽出データ
     SELECT   KEISYKL1  ASSIGN    TO        DA-01-VI-KEISYKL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       KEI-F07   KEI-F23
                                            KEI-F051  KEI-F03
                        FILE  STATUS   IS   KEI-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    売上累積データ
******************************************************************
 FD  KEIURIL3           LABEL RECORD   IS   STANDARD.
     COPY               KEIURIF        OF   XFDLIB
                        JOINING        URI  PREFIX.
*
******************************************************************
*    抽出売上累積データ
******************************************************************
 FD  KEISYKL1           LABEL RECORD   IS   STANDARD.
     COPY               KEISYKF        OF   XFDLIB
                        JOINING        KEI  PREFIX.
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  SET-FLG                 PIC  X(03)     VALUE  SPACE.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WT-CNT                  PIC  9(08)     VALUE  ZERO.
 01  AKAKURO-CNT             PIC  9(08)     VALUE  ZERO.
 01  AKA-CNT                 PIC  9(08)     VALUE  ZERO.
 01  WK-URI-F112             PIC  9(08)     VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  URI-STATUS        PIC  X(02).
     03  KEI-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSK0035B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSK0035B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSK0035B".
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
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME            PIC   9(08)  VALUE  ZERO.
*
 LINKAGE                SECTION.
 01  PARA-SKBN              PIC   X(01).
 01  PARA-DKBN              PIC   X(01).
 01  PARA-DFROM             PIC   9(08).
 01  PARA-DTO               PIC   9(08).
 01  PARA-KKBN              PIC   X(01).
 01  PARA-TANFROM           PIC   X(02).
 01  PARA-TANTO             PIC   X(02).
 01  PARA-DENK1             PIC   X(02).
 01  PARA-DENK2             PIC   X(02).
 01  PARA-DENK3             PIC   X(02).
 01  PARA-DENK4             PIC   X(02).
 01  PARA-DENK5             PIC   X(02).
 01  PARA-TENFROM           PIC   9(05).
 01  PARA-TENTO             PIC   9(05).
 01  PARA-DENNFROM          PIC   9(09).
 01  PARA-DENNTO            PIC   9(09).
 01  PARA-SKBFROM           PIC   X(02).
 01  PARA-SKBTO             PIC   X(02).
 01  PARA-DENKFROM          PIC   X(02).
 01  PARA-DENKTO            PIC   X(02).
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION  USING    PARA-SKBN
                                           PARA-DKBN
                                           PARA-DFROM
                                           PARA-DTO
                                           PARA-KKBN
                                           PARA-TANFROM
                                           PARA-TANTO
                                           PARA-DENK1
                                           PARA-DENK2
                                           PARA-DENK3
                                           PARA-DENK4
                                           PARA-DENK5
                                           PARA-TENFROM
                                           PARA-TENTO
                                           PARA-DENNFROM
                                           PARA-DENNTO
                                           PARA-SKBFROM
                                           PARA-SKBTO
                                           PARA-DENKFROM
                                           PARA-DENKTO.
****************************************************************
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KEIURIL3.
     MOVE      "KEIURIL3"   TO   AB-FILE.
     MOVE      URI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KEISYKL1.
     MOVE      "KEISYKL1"   TO   AB-FILE.
     MOVE      KEI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
****************************************************************
*              コントロール                                    *
****************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FLG   =   "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
*
     OPEN     I-O      KEISYKL1.
     OPEN     INPUT    KEIURIL3.
*
     DISPLAY  MSG-START UPON CONS.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB" USING     LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*売上累積Ｄスタート
     MOVE    173           TO        URI-F01.
     MOVE    PARA-DFROM    TO        URI-F112.
     START   KEIURIL3 KEY  IS    >=  URI-F01  URI-F112
             INVALID
             MOVE   "END"  TO        END-FLG
             DISPLAY
             NC"＃＃　売上累積Ｄ対象無　１　＃＃"   UPON CONS
             MOVE     4001          TO   PROGRAM-STATUS
             STOP     RUN
     END-START.
*売上累積データ読み込み
     PERFORM KEIURIF-READ-SEC.
     IF      "END"     =  END-FLG
             DISPLAY
             NC"＃＃　売上累積Ｄ対象無　２　＃＃"   UPON CONS
             MOVE     4001          TO   PROGRAM-STATUS
             STOP     RUN
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
     MOVE    URI-F07      TO KEI-F07.
     MOVE    URI-F23      TO KEI-F23.
     MOVE    URI-F051     TO KEI-F051.
     MOVE    URI-F03      TO KEI-F03.
     READ    KEISYKL1
             INVALID
             MOVE   URI-REC      TO   KEI-REC
             WRITE  KEI-REC
             ADD    1            TO   WT-CNT
             NOT INVALID
             ADD    1            TO   AKAKURO-CNT
     END-READ.
*売上累積データ読み込み
     PERFORM KEIURIF-READ-SEC.
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
     DISPLAY   "#READ-CNT  = "  RD-CNT   " #"  UPON  CONS.
     DISPLAY   "#WRITE-CNT = "  WT-CNT   " #"  UPON  CONS.
*
     CLOSE     KEIURIL3  KEISYKL1.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　　売上累積データ読込
****************************************************************
 KEIURIF-READ-SEC        SECTION.
*
     MOVE  "KEIURIF-READ-SEC"     TO    S-NAME.
*
     READ  KEIURIL3
           AT END
           MOVE   "END"           TO    END-FLG
           GO                     TO    KEIURIF-READ-EXIT
           NOT AT END
           ADD    1               TO    RD-CNT
     END-READ.
*
*カウンター表示
     IF    RD-CNT(6:3)  =  "000" OR "500"
           DISPLAY "READ-CNT  = "  RD-CNT  UPON CONS
     END-IF.
*売上累積データ：取引先ＣＤが１７３より大きい
     IF    URI-F01      >  173
           MOVE    "END"          TO    END-FLG
     END-IF.
*行番号＝８０は処理対象としない
     IF    URI-F03  =  80
           GO                     TO    KEIURIF-READ-SEC
     END-IF.
*納品日範囲チェック
     MOVE  URI-F112               TO    WK-URI-F112.
     IF    WK-URI-F112  >  PARA-DTO
           MOVE    "END"          TO    END-FLG
     END-IF.
*
 KEIURIF-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
