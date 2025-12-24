# SSE0010I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SSE0010I.COB`

## ソースコード

```cobol
****************************************************************
*            請求処理　締日入力
*            PROGRAM ---> SSE0010I
*            FORM    ---> FSE00101
****************************************************************
 IDENTIFICATION        DIVISION.
****************************************************************
 PROGRAM-ID.                 SSE0010I.
 AUTHOR.                     NAV.
 DATE-WRITTEN.               92/12/17.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION               SECTION.
 SOURCE-COMPUTER.
 OBJECT-COMPUTER.
 SPECIAL-NAMES.
     CONSOLE                 IS  CONSL
     STATION                 IS  STAT.
 INPUT-OUTPUT                SECTION.
 FILE-CONTROL.
*--< 条件ファイル >--*
     SELECT  HJYOKEN         ASSIGN  TO  DA-01-VI-JYOKEN1
                             ORGANIZATION          IS  INDEXED
                             ACCESS    MODE        IS  RANDOM
                             RECORD  KEY IS    JYO-F01 JYO-F02
                             FILE        STATUS    IS  JYO-ST.
*--< 画面ファイル >--*
     SELECT  DSPFILE         ASSIGN  TO  01-GS-DSPF
                             SYMBOLIC  DESTINATION IS  "DSP"
                             DESTINATION-1         IS  DSP-WS
                             FORMAT                IS  DSP-FMT
                             GROUP                 IS  DSP-GRP
                             PROCESSING  MODE      IS  DSP-PRO
                             UNIT        CONTROL   IS  DSP-UNIT
                             SELECTED    FUNCTION  IS  DSP-FNC
                             FILE        STATUS    IS  DSP-ST.
****************************************************************
 DATA    DIVISION.
****************************************************************
 FILE    SECTION.
*--------------------------<< 条件ファイル >>
 FD  HJYOKEN
     LABEL  RECORD           IS  STANDARD.
*
     COPY   HJYOKEN      OF      XFDLIB
            JOINING      JYO     PREFIX.
*--------------------------<< ＤＩＳＰＬＡＹ >>
 FD  DSPFILE.
*
     COPY   FSE00101   OF   XMDLIB.
****************************************************************
 WORKING-STORAGE       SECTION.
****************************************************************
 77  IN-DATA                 PIC  X(01)   VALUE  SPACE.
 01  ERR-F                   PIC  9(01)   VALUE  ZERO.
 01  END-F                   PIC  9(01)   VALUE  ZERO.
 01  JYO-ST                  PIC  X(02)   VALUE  SPACE.
*
 01  DSP-AREA.
     03  DSP-FMT             PIC  X(08)  VALUE   SPACE.
     03  DSP-GRP             PIC  X(08)  VALUE   SPACE.
     03  DSP-WS              PIC  X(08)  VALUE   SPACE.
     03  DSP-WSR  REDEFINES  DSP-WS.
         05  DSP-WS1         PIC  X(02).
         05  DSP-WS2         PIC  9(03).
         05  DSP-WS3         PIC  X(01).
     03  DSP-PRO             PIC  X(02)  VALUE   SPACE.
     03  DSP-UNIT            PIC  X(06)  VALUE   SPACE.
     03  DSP-FNC             PIC  X(04)  VALUE   SPACE.
     03  DSP-ST              PIC  X(02)  VALUE   SPACE.
 01  WORK-AREA.
     03  WK-57               PIC  9(04).
     03  WK-59               PIC  9(08).
     03  WK-59R      REDEFINES   WK-59.
         05  WK-59Y          PIC  9(04).
         05  WK-59M          PIC  9(02).
         05  WK-59D          PIC  9(02).
     03  WK-R01              PIC  9(08).
     03  WK-R01R     REDEFINES   WK-R01.
         05  WK-R01Y         PIC  9(04).
         05  WK-R01M         PIC  9(02).
         05  WK-R01D         PIC  9(02).
*
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                 PIC  9(08).
*画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
*
 01  ERR-AREA.
     03  ERRA.
         05  ERR01               PIC  N(20)     VALUE
                   NC"日付エラーです！".
     03  ERRB        REDEFINES   ERRA.
         05  ERR-MSG         PIC  N(20)     OCCURS 1.
*
 01  FILE-ERR.
     03  JYOERR              PIC  N(15)     VALUE
                   NC"条件ファイル　異常！".
     03  DSPERR              PIC  N(15)     VALUE
                   NC"画面ファイル　異常！".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
****************************************************************
 PROCEDURE                   DIVISION.
 DECLARATIVES.
 JYO-ERR                SECTION.
     USE AFTER         EXCEPTION      PROCEDURE HJYOKEN.
     DISPLAY      JYOERR JYO-ST       UPON STAT.
     ACCEPT       IN-DATA             FROM STAT.
     STOP         RUN.
 DSP-ERR                SECTION.
     USE AFTER         EXCEPTION      PROCEDURE DSPFILE.
     DISPLAY      DSPERR DSP-ST       UPON STAT.
     ACCEPT       IN-DATA             FROM STAT.
     STOP         RUN.
 END DECLARATIVES.
****************************************************************
*             PROGRAM-SEC
****************************************************************
 PROGRAM-SEC                SECTION.
     PERFORM           INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL    END-F     =    9.
     PERFORM           END-SEC.
     STOP                   RUN.
*PROGRAM-END.
****************************************************************
*             INIT-SEC
****************************************************************
 INIT-SEC                   SECTION.
     OPEN   I-O              HJYOKEN.
     OPEN   I-O              DSPFILE.
*
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
*画面表示日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*
     MOVE   59               TO   JYO-F01.
     MOVE   SPACE            TO   JYO-F02.
     READ   HJYOKEN  INVALID   KEY
         DISPLAY NC"条件ファイル未登録　ＫＥＹ＝５９" UPON STAT
         ACCEPT  IN-DATA                              FROM STAT
         MOVE    9           TO   END-F
         GO                  TO   INIT-EXIT.
*
     MOVE   JYO-F04          TO   WK-59.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             MAIN-SEC
****************************************************************
 MAIN-SEC                   SECTION.
 MAIN-010.
     PERFORM  DSP-INIT-SEC   THRU DSP-INIT-EXIT.
*--< 処理日　表示 >--*
     MOVE     WK-59Y(3:2)    TO   R01Y.
     MOVE     WK-59M         TO   R01M.
     MOVE     WK-59D         TO   R01D.
     MOVE     "GR01"         TO   DSP-GRP.
     PERFORM  DSP-WR-SEC     THRU DSP-WR-EXIT.
*--< 処理日　入力 >--*
 MAIN-020.
     MOVE     "GR01"         TO   DSP-GRP.
     PERFORM  DSP-RD-SEC     THRU DSP-RD-EXIT.
     EVALUATE      DSP-FNC
         WHEN      "F006"
                             GO   TO   MAIN-010
         WHEN      "F016"
                             MOVE 4010 TO   PROGRAM-STATUS
                             MOVE   9  TO   END-F
                             GO   TO   MAIN-EXIT
         WHEN      "E000"
                             CONTINUE
         WHEN      OTHER
                             GO   TO   MAIN-020
     END-EVALUATE.
     PERFORM  DSP-WR-SEC     THRU DSP-WR-EXIT.
*--< 処理日　ＣＨＫ >--*
     MOVE     R01Y      TO   WK-R01Y.
     MOVE     R01M      TO   WK-R01M.
     MOVE     R01D      TO   WK-R01D.
*
     PERFORM  YMD-CHK-SEC    THRU YMD-CHK-EXIT.
     IF       ERR-F     NOT  =    ZERO
              PERFORM        ERR-WR-SEC
              GO   TO   MAIN-020.
     PERFORM        ERR-CL-SEC.
 MAIN-030.
*--< 確認 >--*
     MOVE   "RKAKU"          TO   DSP-GRP.
     PERFORM  DSP-RD-SEC     THRU DSP-RD-EXIT.
     EVALUATE      DSP-FNC
         WHEN      "F006"
                             GO   TO   MAIN-020
         WHEN      "F016"
                             MOVE   4010    TO   PROGRAM-STATUS
                             MOVE   9       TO   END-F
                             GO   TO   MAIN-EXIT
         WHEN      "E000"
                             CONTINUE
         WHEN      OTHER
                             GO   TO   MAIN-030
     END-EVALUATE.
 MAIN-040.
*--< 条件Ｆ　更新 >--*
**   IF       WK-R01Y   <    90
**            COMPUTE   WK-R01Y   =    WK-R01Y   +    2000
**   ELSE
**            COMPUTE   WK-R01Y   =    WK-R01Y   +    1900
**   END-IF.
     MOVE     WK-R01     TO   JYO-F04.
     REWRITE  JYO-REC.
     MOVE     9          TO   END-F.
 MAIN-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                     SECTION.
     CLOSE                   HJYOKEN.
     CLOSE                   DSPFILE.
 END-EXIT.
     EXIT.
****************************************************************
*    日付－ＣＨＥＣＫ
****************************************************************
 YMD-CHK-SEC            SECTION.
     MOVE  ZERO              TO   ERR-F.
*--< 常識　ＣＨＫ >--*
     IF       WK-R01Y   <    90
              COMPUTE   WK-R01Y   =    WK-R01Y   +    2000
     ELSE
              COMPUTE   WK-R01Y   =    WK-R01Y   +    1900
     END-IF.
     IF  ( WK-R01Y            = ZERO  )  OR
         ( WK-R01M            = ZERO  )  OR
         ( WK-R01M            >   12  )  OR
         ( WK-R01D            = ZERO  )  OR
         ( WK-R01D            >   31  )
         MOVE      1         TO   ERR-F
         GO                  TO   YMD-CHK-EXIT.
*    MOVE     "3"                 TO   LINK-IN-KBN.
*    MOVE     WK-R01              TO   LINK-IN-YMD6.
*    MOVE     ZERO                TO   LINK-IN-YMD8.
*    MOVE     ZERO                TO   LINK-OUT-RET.
*    MOVE     ZERO                TO   LINK-OUT-YMD.
*    CALL     "SKYDTCKB"       USING   LINK-IN-KBN
*                                      LINK-IN-YMD6
*                                      LINK-IN-YMD8
*                                      LINK-OUT-RET
*                                      LINK-OUT-YMD.
*    DISPLAY "LINK-OUT-YMD = " LINK-OUT-YMD UPON CONSL.
*    IF       LINK-OUT-RET  = ZERO
*             MOVE  LINK-OUT-YMD  TO   WK-R01
*    ELSE
*             MOVE  1             TO   ERR-F
*    END-IF.
 YMD-CHK-EXIT.
     EXIT.
****************************************************************
*    画面　　ＲＥＡＤ　　
****************************************************************
 DSP-RD-SEC             SECTION.
     MOVE  "NE"               TO  DSP-PRO.
     READ                         DSPFILE.
 DSP-RD-EXIT.
     EXIT.
****************************************************************
*    画面　　ＷＲＩＴＥ　
****************************************************************
 DSP-WR-SEC             SECTION.
     MOVE  SPACE              TO  DSP-PRO.
     WRITE                        FSE00101.
 DSP-WR-EXIT.
     EXIT.
****************************************************************
*    エラー　　表示
****************************************************************
 ERR-WR-SEC           SECTION.
     MOVE    ERR-MSG ( ERR-F )    TO   WERR.
     MOVE     "WERR"         TO   DSP-GRP.
     PERFORM  DSP-WR-SEC     THRU DSP-WR-EXIT.
 ERR-WR-EXIT.
     EXIT.
****************************************************************
*    エラー　　クリアー
****************************************************************
 ERR-CL-SEC           SECTION.
     MOVE     SPACE          TO   WERR.
     MOVE     "WERR"         TO   DSP-GRP.
     PERFORM  DSP-WR-SEC     THRU DSP-WR-EXIT.
 ERR-CL-EXIT.
     EXIT.
****************************************************************
*    初期画面　表示
****************************************************************
 DSP-INIT-SEC           SECTION.
     MOVE    SPACE           TO   FSE00101.
     MOVE    SPACE           TO   DSP-AREA.
     MOVE   "FSE00101"          TO   DSP-FMT.
     MOVE   "SCREEN"         TO   DSP-GRP.
     MOVE   "CL"             TO   DSP-PRO.
     MOVE    HEN-DATE        TO   SDATE.
     MOVE    HEN-TIME        TO   STIME.
     WRITE   FSE00101.
 DSP-INIT-EXIT.
     EXIT.

```
