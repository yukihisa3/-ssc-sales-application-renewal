****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ケーヨー伝票レス                  *
*    モジュール名　　　　：　出荷実績データ抽出（マッチング用）*
*    作成日／作成者　　　：　2014/03/26  YOSHIDA.M             *
*    処理概要　　　　　　：　納品日範囲のパラメタを受取り、ケー*
*                            ヨーで売上伝票ファイルにスタートを*
*                            掛け、パラメタ納品日範囲のデータを*
*                            出荷実績抽出データに出力する。    *
*                            また、同一伝票内で複数の相殺伝票が*
*                            存在する場合、最終の相殺区分の伝票*
*                            のみを出力対象とする。            *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SSK0031B.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*売上伝票ファイル
     SELECT   SHTDENLP  ASSIGN    TO        DA-01-VI-SHTDENLP
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DEN-F01   DEN-F051
                                            DEN-F07   DEN-F112
                                            DEN-F02   DEN-F03
                                            DEN-F04
                        FILE  STATUS   IS   DEN-STATUS.
*出荷実績抽出データ
     SELECT   KEISYKF   ASSIGN    TO        DA-01-S-KEISYKF
                        ORGANIZATION        SEQUENTIAL
                        FILE  STATUS   IS   KEI-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    売上伝票ファイル
******************************************************************
 FD  SHTDENLP           LABEL RECORD   IS   STANDARD.
     COPY               SHTDENF        OF   XFDLIB
                        JOINING        DEN  PREFIX.
*
******************************************************************
*    抽出売上伝票ファイル
******************************************************************
 FD  KEISYKF            LABEL RECORD   IS   STANDARD.
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
 01  WK-DEN-F112             PIC  9(08)     VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  KEI-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSK0031B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSK0031B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSK0031B".
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
                        PROCEDURE   SHTDENLP.
     MOVE      "SHTDENLP"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KEISYKF.
     MOVE      "KEISYKF"    TO   AB-FILE.
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
     OPEN     INPUT    SHTDENLP
     OPEN     OUTPUT   KEISYKF.
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
*売上伝票Ｆスタート
     MOVE    173           TO        DEN-F01.
     MOVE    ZERO          TO        DEN-F051.
     MOVE    ZERO          TO        DEN-F07.
     MOVE    ZERO          TO        DEN-F112.
     MOVE    ZERO          TO        DEN-F02.
     MOVE    ZERO          TO        DEN-F03.
     MOVE    ZERO          TO        DEN-F04.
     START   SHTDENLP KEY  IS    >=  DEN-F01  DEN-F051
                                     DEN-F07  DEN-F112
                                     DEN-F02  DEN-F03
                                     DEN-F04
             INVALID
             MOVE   "END"  TO        END-FLG
             DISPLAY
             NC"＃＃　売上伝票Ｆ対象無　１　＃＃"   UPON CONS
             MOVE     4001          TO   PROGRAM-STATUS
             STOP     RUN
     END-START.
*売上伝票ファイル読み込み
     PERFORM SHTDENF-READ-SEC.
     IF      "END"     =  END-FLG
             DISPLAY
             NC"＃＃　売上伝票Ｆ対象無　２　＃＃"   UPON CONS
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
     IF  DEN-F01  = KEI-F01  AND
         DEN-F051 = KEI-F051 AND
         DEN-F07  = KEI-F07  AND
         DEN-F112 = KEI-F112 AND
         DEN-F02  = KEI-F02  AND
         DEN-F03  = KEI-F03  THEN
         ADD    1                TO   AKAKURO-CNT
     ELSE
         IF  SET-FLG = "SET"
             MOVE   KEI-F23      TO   KEI-F02
             WRITE  KEI-REC
             ADD    1            TO   WT-CNT
             MOVE   SPACE        TO   SET-FLG
         END-IF
     END-IF.
     IF  DEN-F04 = 0 OR 2 OR 4 OR 6 OR 8 THEN
         INITIALIZE                   KEI-REC
         MOVE   DEN-REC          TO   KEI-REC
         MOVE   "SET"            TO   SET-FLG
     ELSE
         ADD    1                TO   AKA-CNT
         MOVE   SPACE            TO   SET-FLG
     END-IF.
*売上伝票ファイル読み込み
     PERFORM SHTDENF-READ-SEC.
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
     IF       SET-FLG = "SET"  THEN
              WRITE  KEI-REC
              MOVE   SPACE       TO    SET-FLG
              ADD    1           TO    WT-CNT
     END-IF
*
     DISPLAY   "#READ-CNT  = "  RD-CNT   " #"  UPON  CONS.
     DISPLAY   "#WRITE-CNT = "  WT-CNT   " #"  UPON  CONS.
*
     CLOSE     SHTDENLP  KEISYKF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　　売上伝票ファイル読込
****************************************************************
 SHTDENF-READ-SEC        SECTION.
*
     MOVE  "SHTDENF-READ-SEC"     TO    S-NAME.
*
     READ  SHTDENLP
           AT END
           MOVE   "END"           TO    END-FLG
           GO                     TO    SHTDENF-READ-EXIT
           NOT AT END
           ADD    1               TO    RD-CNT
     END-READ.
*
*カウンター表示
     IF    RD-CNT(6:3)  =  "000" OR "500"
           DISPLAY "READ-CNT  = "  RD-CNT  UPON CONS
     END-IF.
*売上伝票ファイル：取引先ＣＤが１７３より大きい
     IF    DEN-F01      >  173
           MOVE    "END"          TO    END-FLG
     END-IF.
*行番号＝８０は抽出対象としない。
     IF    DEN-F03  =  80
           GO                     TO    SHTDENF-READ-SEC
     END-IF.
*納品日範囲チェック
     MOVE  DEN-F112               TO    WK-DEN-F112.
     IF    WK-DEN-F112  <  PARA-DFROM    OR
           WK-DEN-F112  >  PARA-DTO
           GO                     TO    SHTDENF-READ-SEC
     END-IF.
*
 SHTDENF-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
