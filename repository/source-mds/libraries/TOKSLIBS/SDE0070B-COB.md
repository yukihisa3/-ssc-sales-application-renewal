# SDE0070B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SDE0070B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　伝票ＥＸＣＥＬ連携　　　          *
*    モジュール名　　　　：　伝票番号採番　　　　　　　　　　　*
*    作成日／作成者　　　：　2016/09/28 INOUE                  *
*    処理概要　　　　　　：　基本売上伝票ファイルに　　　　　　*
*                            伝票番号を採番する。　　　　　　　*
*    更新日／更新者　　　：　                                  *
*    更新概要　　　　　　：　                                  *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SDE0070B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2016/09/28.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*基本売上伝票ファイル
     SELECT   URIXXXL1  ASSIGN    TO        DA-01-VI-URIXXXL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       URI-F70   URI-F01
                                            URI-F051  URI-F02
                                            URI-F07   URI-F03
                        WITH      DUPLICATES
                        FILE      STATUS IS URI-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    基本売上伝票ファイル
******************************************************************
 FD  URIXXXL1            LABEL RECORD   IS   STANDARD.
     COPY     URIXXXL1   OF        XFDLIB
              JOINING    URI       PREFIX.
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
*FLG/ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  ZERO.
 01  URI-FLG                 PIC  X(03)     VALUE  ZERO.
*01  DEL-FLG                 PIC  X(03)     VALUE  ZERO.
 01  URIXXXL1-UP-CNT         PIC  9(07)     VALUE  ZERO.
*01  ERR-CNT                 PIC  9(07)     VALUE  ZERO.
 01  URIXXXL1-READ-CNT       PIC  9(07)     VALUE  ZERO.
*01  GYO-CNT                 PIC  9(02)     VALUE  ZERO.
*01  INIT-FLG                PIC  X(01)     VALUE  "1".
 01  TOK-INV-FLG             PIC  X(03)     VALUE  SPACE.
*
 01  WRK-AREA.
     03  OUT-DENNO           PIC  9(09)     VALUE  ZERO.
 01  LINK-AREA-SUB1.
     03  LI-TRCD             PIC  9(08).
     03  LO-ERR              PIC  9(01).
     03  LO-DENNO            PIC  9(09).
     03  LO-NEXT             PIC  9(09).
*
*    03  WRK-TEISUU          PIC S9(09)V99  VALUE  ZERO.
*    03  WRK-MAETEISUU       PIC S9(09)V99  VALUE  ZERO.
*行0保管ワーク
*    COPY     URIXXXL1   OF        XFDLIB
*             JOINING    SAV       PREFIX.
*ブレイクキー
*01  BRK-AREA.
*    03  BRK-F203          PIC X(01)         VALUE SPACE.
*    03  BRK-F204          PIC 9(08)         VALUE ZERO.
*    03  BRK-F207          PIC X(02)         VALUE SPACE.
*    03  BRK-F205          PIC 9(09)         VALUE ZERO.
*    03  BRK-F208          PIC 9(05)         VALUE ZERO.
*    03  BRK-F206          PIC 9(02)         VALUE ZERO.
*計算領域
 01  WRK-AREA2.
     03  WRK-HIK           PIC S9(09)V9(02)  VALUE ZERO.
     03  WRK-ZAI           PIC S9(09)V9(02)  VALUE ZERO.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
*システム時刻の編集
     03  SYSTIME           PIC  9(08)  VALUE  ZERO.
     03  SYSTIMER          REDEFINES   SYSTIME.
         05  SYS-TIME1     PIC  9(06).
         05  SYS-TIME2     PIC  9(02).
 01  WK-ST.
     03  URI-STATUS        PIC  X(02).
     03  NFM-STATUS        PIC  X(02).
     03  TBL-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
     03  TOK-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SDE0070B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SDE0070B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SDE0070B".
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
*日付サブルーチン用
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
*LINKAGE                SECTION.
*01  PARA-IN-TANCD      PIC   X(02).
*01  PARA-IN-BUMCD      PIC   X(04).
*01  PARA-OUT-DATE      PIC   9(08).
*01  PARA-OUT-TIME      PIC   9(06).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   URIXXXL1.
     MOVE      "URIXXXL1"   TO   AB-FILE.
     MOVE      URI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC   UNTIL  END-FLG = "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     INITIALIZE                   WRK-AREA.
     OPEN     I-O       URIXXXL1.
*
     DISPLAY  MSG-START UPON CONS.
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
     ACCEPT      SYSTIME   FROM      TIME.
*    MOVE        SYS-DATEW TO        PARA-OUT-DATE.
*    MOVE        SYS-TIME1 TO        PARA-OUT-TIME.
*基本売上伝票ファイル対象レコード有無判定
     PERFORM     URIXXXL1-START-SEC.
     IF    URI-FLG = "END"
           DISPLAY "＃処理対象がありません！＃" UPON CONS
           MOVE      "END"       TO      END-FLG
           MOVE      4001        TO      PROGRAM-STATUS
           GO                    TO      INIT-EXIT
     END-IF.
*基本売上伝票ファイル読込(1件目)
     PERFORM     URIXXXL1-READ-SEC.
     IF    URI-FLG = "END"
           DISPLAY "＃処理対象がありません！＃" UPON CONS
           MOVE      "END"       TO      END-FLG
           MOVE      4001        TO      PROGRAM-STATUS
           GO                    TO      INIT-EXIT
     END-IF.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*
*伝票番号採番(行＝１の時）
     IF    URI-F03      =  1
           PERFORM DPNO-SET-SEC
     END-IF.
*
*伝票番号セット・更新
     MOVE    OUT-DENNO          TO  URI-F02.
     REWRITE URI-REC.
     ADD     1                  TO  URIXXXL1-UP-CNT.
*
 MAIN-010.
     PERFORM  URIXXXL1-READ-SEC.
*
     IF  URI-FLG  = "END"
         MOVE       "END"        TO   END-FLG
     END-IF.
*
 MAIN-EXIT.
     EXIT.
*
****************************************************************
*　　基本売上伝票ファイル　スタート
****************************************************************
 URIXXXL1-START-SEC     SECTION.
*
     MOVE    "URIXXXL1-READ-SEC"  TO   S-NAME.
*
     MOVE     SPACE              TO   URI-REC.
     INITIALIZE                       URI-REC.
*
     MOVE     " "                TO   URI-F70.
     MOVE     ZERO               TO   URI-F01.
     MOVE     ZERO               TO   URI-F051.
     MOVE     ZERO               TO   URI-F02.
     MOVE     ZERO               TO   URI-F07.
     MOVE     ZERO               TO   URI-F03.
*
     START  URIXXXL1  KEY  IS  >= URI-F70  URI-F01  URI-F051
                                  URI-F02  URI-F07  URI-F03
           INVALID
           MOVE  "END"           TO      URI-FLG
           DISPLAY "＃処理対象がありません！＃" UPON CONS
           MOVE      4001        TO      PROGRAM-STATUS
           GO                    TO      URIXXXL1-START-EXIT
     END-START.
*
 URIXXXL1-START-EXIT.
     EXIT.
****************************************************************
*　　基本売上伝票ファイル READ
****************************************************************
 URIXXXL1-READ-SEC            SECTION.
*
     MOVE    "URIXXXL1-READ-SEC" TO        S-NAME.
*
     READ  URIXXXL1  NEXT AT  END
           MOVE  "END"          TO        URI-FLG
           GO                   TO        URIXXXL1-READ-EXIT
     END-READ.
 READ010.
*制御区分チェック
     IF       URI-F70  NOT =  " "
              MOVE     "END"      TO   URI-FLG
              GO                  TO   URIXXXL1-READ-EXIT
     END-IF.
 READ020.
*件数カウント
     ADD      1                   TO   URIXXXL1-READ-CNT.
*↓TEST
     IF       URIXXXL1-READ-CNT < 20
        DISPLAY "READ DENNO=" URI-F02 UPON CONS
        DISPLAY "     GYONO=" URI-F03 UPON CONS
     END-IF.
*↑TEST
*
 URIXXXL1-READ-EXIT.
     EXIT.
******************************************************************
*                  伝票番号取得
******************************************************************
 DPNO-SET-SEC                 SECTION.
     MOVE     "DPNO-SET-SEC"       TO   S-NAME.
*
     INITIALIZE                        LINK-AREA-SUB1.
*
 DPNO-010.
     MOVE     URI-F01        TO        LI-TRCD.
*↓TEST
*    DISPLAY   "URI-F01="  URI-F01   UPON CONS.
*    DISPLAY   "LI-TRCD="  LI-TRCD   UPON CONS.
*↑TEST
     CALL     "SKYSBCK1"     USING     LINK-AREA-SUB1.
 DPNO-020.
*↓TEST
*    DISPLAY   "LO-ERR ="  LO-ERR    UPON CONS.
*    DISPLAY   "LO-DENO="  LO-DENNO  UPON CONS.
*    DISPLAY   "LO-NEXT="  LO-NEXT   UPON CONS.
*↑TEST
     IF       LO-ERR  =  0
              MOVE      LO-NEXT     TO        OUT-DENNO
              MOVE      SPACE       TO        TOK-INV-FLG
     ELSE
              DISPLAY   NC"伝票_採番エラー"  UPON CONS
              DISPLAY   LI-TRCD               UPON CONS
              MOVE      "INV"       TO        TOK-INV-FLG
              MOVE      4001        TO        PROGRAM-STATUS
*             ACCEPT    IN-DATA     FROM      CONS
              STOP  RUN
     END-IF.
*
 DPNO-SET-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*件数印字
*基本売上伝票ファイル読込
     DISPLAY "URIXXXL1   READ CNT = " URIXXXL1-READ-CNT UPON CONS.
*基本売上伝票ファイル更新
     DISPLAY "URIXXXL1   UPDT CNT = " URIXXXL1-UP-CNT   UPON CONS.
*
     CLOSE    URIXXXL1.
*
     STOP     RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
