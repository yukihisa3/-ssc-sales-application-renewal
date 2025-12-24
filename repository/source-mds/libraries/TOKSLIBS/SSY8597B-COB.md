# SSY8597B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY8597B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＳＯＳ処理                        *
*    業務名　　　　　　　：　くろがねや出荷検品                *
*    モジュール名　　　　：　出荷送信済ＦＬＧ解除（基本情報）  *
*    作成日／更新日　　　：　2006/12/11                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　指定バッチ番号＋倉庫ＣＤの出荷デー*
*                            タの送信ＦＬＧを解除する。        *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY8597B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          06/12/11.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*くろがねや出荷情報データ
     SELECT   KGJOHOF   ASSIGN    TO        DA-01-VI-KGJOHOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       KMK-F01   KMK-F02
                                            KMK-F03   KMK-F04
                                            KMK-F05   KMK-F06
                                            KMK-F07
                        FILE      STATUS    KMK-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    くろがねや出荷確定データ
******************************************************************
 FD  KGJOHOF
                        LABEL RECORD   IS   STANDARD.
     COPY     KGJOHOF   OF        XFDLIB
              JOINING   KMK  AS   PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  WK-DEN-CNT          PIC  9(08)     VALUE  ZERO.
     03  WK-REC-CNT          PIC  9(08)     VALUE  ZERO.
     03  HEN1-CNT            PIC  9(08)     VALUE  ZERO.
     03  HEN2-CNT            PIC  9(08)     VALUE  ZERO.
 01  WK-KEY-JYOHO.
     03  WK-KMK-F09A         PIC  9(08)     VALUE  ZERO.
     03  WK-KMK-F05          PIC  9(05)     VALUE  ZERO.
     03  WK-KMK-F06          PIC  9(09)     VALUE  ZERO.
 01  WK-INV-FLG.
     03  KGJOHOF-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  SHTDENF-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  KMJOHOF-INV-FLG     PIC  X(03)     VALUE  SPACE.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  KMK-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY8597B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY8597B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY8597B".
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
 LINKAGE                    SECTION.
 01  PARA-HIDUKE            PIC 9(08).
 01  PARA-JIKAN             PIC 9(04).
 01  PARA-TORICD            PIC 9(08).
 01  PARA-SOKCD             PIC X(02).
 01  PARA-NOUDT             PIC 9(08).
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION   USING  PARA-HIDUKE
                                          PARA-JIKAN
                                          PARA-TORICD
                                          PARA-SOKCD
                                          PARA-NOUDT.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KGJOHOF.
     MOVE      "KGJOHOL1"   TO   AB-FILE.
     MOVE      KMK-STATUS   TO   AB-STS.
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
              UNTIL     END-FLG   =  "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     I-O       KGJOHOF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FLG   WK-CNT.
     MOVE     SPACE     TO        WK-INV-FLG.
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
*くろがねや出荷確定データ読込み
     MOVE        PARA-HIDUKE    TO   KMK-F01.
     MOVE        PARA-JIKAN     TO   KMK-F02.
     MOVE        PARA-TORICD    TO   KMK-F03.
     MOVE        PARA-SOKCD     TO   KMK-F04.
     MOVE        ZERO           TO   KMK-F05.
     MOVE        ZERO           TO   KMK-F06.
     MOVE        ZERO           TO   KMK-F07.
     START  KGJOHOF  KEY   >=  KMK-F01 KMK-F02 KMK-F03 KMK-F04
                                  KMK-F05 KMK-F06 KMK-F07
            INVALID  KEY
            DISPLAY NC"対象データ無し" UPON CONS
            STOP  RUN
     END-START.
*    くろがねや出荷確定データ読込み
     PERFORM KGJOHOF-READ-SEC.
*    終了条件判定
     IF       END-FLG  NOT =  "END"
              MOVE     KMK-F09A   TO   WK-KMK-F09A
              MOVE     KMK-F05    TO   WK-KMK-F05
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*    送信ＦＬＧ解除
     MOVE     ZERO                TO   KMK-F13.
     REWRITE  KMK-REC.
*
     ADD      1                   TO   HEN1-CNT.
*    くろがねや出荷確定データ読込み
     PERFORM  KGJOHOF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　カーマ出荷確定データ読込み　　　　　　　　　　　　　　　　*
****************************************************************
 KGJOHOF-READ-SEC    SECTION.
*
     READ     KGJOHOF
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  KGJOHOF-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*    同一キーチェック
     IF       PARA-HIDUKE    =  KMK-F01
     AND      PARA-JIKAN     =  KMK-F02
     AND      PARA-TORICD    =  KMK-F03
     AND      PARA-SOKCD     =  KMK-F04
              CONTINUE
     ELSE
              MOVE     "END"    TO  END-FLG
              GO                TO  KGJOHOF-READ-EXIT
     END-IF.
*    納品日範囲チェック
     IF       PARA-NOUDT  =  ZERO
              CONTINUE
     ELSE
              IF   PARA-NOUDT = KMK-F09B
                   CONTINUE
              ELSE
                   GO           TO  KGJOHOF-READ-SEC
              END-IF
     END-IF.
*
 KGJOHOF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY "ｶｰﾏ ｼｭｯｶｶｸﾃｲ RD CNT = " READ-CNT  UPON CONS.
     DISPLAY "ｶｰﾏ ｿｳｼﾝFLGｶｲｼﾞｮCNT = " HEN1-CNT  UPON CONS.
*
     CLOSE     KGJOHOF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
