# SSY3848B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY3848B.COB`

## ソースコード

```cobol
****************************************************************

*    サブシステム　　　　：　ナフコ出荷支援システム　　　　　　*
*    業務名　　　　　　　：　本発業務　　　　　　　            *
*    モジュール名　　　　：　本発原売価チェックワーク作成　　　*
*    作成日／更新日　　　：　2015/10/13                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　管理番号を受け取り、数量訂正Ｆを  *
*                            読み、本発原売価チェックワークを　*
*                            作成する。　　　　　　　          *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY3848B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2015/10/13.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*数量訂正ファイル
     SELECT   NFSUTEL1  ASSIGN    TO        DA-01-VI-NFSUTEL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       STE-F01
                                            STE-F05
                                            STE-F06
                                            STE-F07
                                            STE-F08
                                            STE-F09
                        FILE  STATUS   IS   STE-STATUS.
*本発原売価チェックファイル
     SELECT   NFGBCKF   ASSIGN    TO        DA-01-VI-NFGBCKL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       GBC-F01
                                            GBC-F02
                                            GBC-F03
                                            GBC-F04
                                            GBC-F05
                                            GBC-F06
                        FILE STATUS    IS   GBC-STATUS.
*ナフコ商品マスタ
     SELECT   NFSHOMS1  ASSIGN    TO        DA-01-VI-NFSHOMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SHO-F01
                        FILE STATUS    IS   SHO-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    数量訂正ファイル
******************************************************************
 FD  NFSUTEL1           LABEL RECORD   IS   STANDARD.
     COPY     NFSUTEL1  OF        XFDLIB
              JOINING   STE       PREFIX.
******************************************************************
*    本発原売価チェックファイル
******************************************************************
 FD  NFGBCKF            LABEL RECORD   IS   STANDARD.
     COPY     NFGBCKF   OF        XFDLIB
              JOINING   GBC  AS   PREFIX.
******************************************************************
*    ナフコ商品マスタ
******************************************************************
 FD  NFSHOMS1           LABEL RECORD   IS   STANDARD.
     COPY     NFSHOMS1  OF        XFDLIB
              JOINING   SHO  AS   PREFIX.
*
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
*FLG/ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  ZERO.
 01  NFSHOMS1-INV-FLG        PIC  X(03)     VALUE  ZERO.
 01  NFGBCKF-INV-FLG         PIC  X(03)     VALUE  ZERO.
*
*システム日付の編集
 01  WK-DATE.
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  STE-STATUS        PIC  X(02).
     03  GBC-STATUS        PIC  X(02).
     03  SHO-STATUS        PIC  X(02).
 01  WK-AREA.
     03  WK-TORAY          PIC  9(07)V9(01)  VALUE  ZERO.
     03  WK-HAKOS          PIC  9(07)V9(01)  VALUE  ZERO.
 01  WK-COUNTER.
     03  RD-CNT            PIC  9(07)        VALUE  ZERO.
     03  WT-CNT            PIC  9(07)        VALUE  ZERO.
     03  RW-CNT            PIC  9(07)        VALUE  ZERO.
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY3848B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3848B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3848B".
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
 LINKAGE                SECTION.
 01  PARA-IN-KANRINO    PIC   9(08).
 01  PARA-IN-SAKUBACD   PIC   X(02).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-IN-KANRINO
                                       PARA-IN-SAKUBACD.
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFSUTEL1.
     MOVE      "NFSUTEL1"   TO   AB-FILE.
     MOVE      STE-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFGBCKF.
     MOVE      "NFGBCKL1"   TO   AB-FILE.
     MOVE      GBC-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFSHOMS1.
     MOVE      "NFSHOMS1"   TO   AB-FILE.
     MOVE      SHO-STATUS   TO   AB-STS.
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
*
     MOVE     "INIT-SEC"          TO   S-NAME.
*
     OPEN     INPUT     NFSUTEL1  NFSHOMS1.
     OPEN     I-O       NFGBCKF.
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
*
 INIT-01.
*数量訂正ファイルスタート
     PERFORM  NFSUTEL1-START-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"＃＃　作成対象データ無１　＃＃"  UPON CONS
          MOVE "END"            TO   END-FLG
          MOVE 4010             TO   PROGRAM-STATUS
          GO                    TO   INIT-EXIT
     END-IF.
*
 INIT-02.
*数量訂正ファイル読込
     PERFORM NFSUTEL1-READ-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"＃＃　作成対象データ無２　＃＃"  UPON CONS
          MOVE "END"            TO   END-FLG
          MOVE 4010             TO   PROGRAM-STATUS
          GO                    TO   INIT-EXIT
     END-IF.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    数量訂正ファイルスタート
****************************************************************
 NFSUTEL1-START-SEC          SECTION.
*
     MOVE    "NFSUTEL1-START-SEC" TO   S-NAME.
*
     MOVE     SPACE               TO   STE-REC.
     INITIALIZE                        STE-REC.
*
     MOVE     PARA-IN-KANRINO     TO   STE-F01.
     MOVE     PARA-IN-SAKUBACD    TO   STE-F05.
*
     START  NFSUTEL1  KEY  IS  >= STE-F01
                                  STE-F05
                                  STE-F06
                                  STE-F07
                                  STE-F08
                                  STE-F09
            INVALID
            MOVE    "END"         TO   END-FLG
     END-START.
*
 NFSUTEL1-START-EXIT.
     EXIT.
*
****************************************************************
*    数量訂正ファイル読込
****************************************************************
 NFSUTEL1-READ-SEC           SECTION.
*
     MOVE    "NFSUTEL1-READ-SEC"  TO   S-NAME.
*
     READ     NFSUTEL1  AT  END
              MOVE     "END"      TO   END-FLG
              GO                  TO   NFSUTEL1-READ-EXIT
     END-READ.
*
     ADD      1                   TO   RD-CNT.
*管理番号チェック
     IF       STE-F01  NOT =  PARA-IN-KANRINO
              MOVE     "END"      TO   END-FLG
              GO                  TO   NFSUTEL1-READ-EXIT
     END-IF.
*作場チェック
     IF       PARA-IN-SAKUBACD  NOT =  "  "
        IF    STE-F05  NOT =  PARA-IN-SAKUBACD
              MOVE     "END"      TO   END-FLG
              GO                  TO   NFSUTEL1-READ-EXIT
     END-IF.
*
 NFSUTEL1-READ-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*
*本発原売価チェックファイル出力
*　存在チェック
     PERFORM NFGBCKF-READ-SEC.
     IF      NFGBCKF-INV-FLG = "INV"
             MOVE     SPACE           TO   GBC-REC
             INITIALIZE                    GBC-REC
             MOVE     STE-F01         TO   GBC-F01
             MOVE     STE-F05         TO   GBC-F02
             MOVE     STE-F08         TO   GBC-F03
             MOVE     STE-F96         TO   GBC-F04
             MOVE     STE-F89         TO   GBC-F05
             MOVE     STE-F90         TO   GBC-F06
             PERFORM NFSHOMS1-READ-SEC
             IF      NFSHOMS1-INV-FLG = SPACE
                     MOVE  SHO-F11    TO   GBC-F07
                     MOVE  SHO-F12    TO   GBC-F08
                     MOVE  SHO-F10    TO   GBC-F12
                     MOVE  SHO-F09    TO   GBC-F13
             ELSE
                     MOVE  1          TO   GBC-F12
                     MOVE  1          TO   GBC-F13
             END-IF
             MOVE     1               TO   GBC-F09
             MOVE     STE-F10         TO   GBC-F10
             MOVE     STE-F11         TO   GBC-F11
             COMPUTE  WK-TORAY  =  GBC-F11  /  GBC-F13 + 0.9
             MOVE     WK-TORAY        TO   GBC-F14
             COMPUTE  WK-HAKOS  =  GBC-F14  /  GBC-F12 + 0.9
             MOVE     WK-HAKOS        TO   GBC-F15
             WRITE  GBC-REC
             ADD      1               TO   WT-CNT
     ELSE
             ADD      1               TO   GBC-F09
             ADD      STE-F11         TO   GBC-F11
             COMPUTE  WK-TORAY  =  GBC-F11  /  GBC-F13 + 0.9
             MOVE     WK-TORAY        TO   GBC-F14
             COMPUTE  WK-HAKOS  =  GBC-F14  /  GBC-F12 + 0.9
             MOVE     WK-HAKOS        TO   GBC-F15
             REWRITE  GBC-REC
             ADD      1               TO   RW-CNT
     END-IF.
*
 MAIN-999.
     PERFORM  NFSUTEL1-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
*
****************************************************************
*    ナフコ商品マスタ読込（存在チェック）
****************************************************************
 NFSHOMS1-READ-SEC           SECTION.
*
     MOVE    "NFSHOMS1-READ-SEC"  TO   S-NAME.
*
     MOVE     STE-F96             TO   SHO-F01.     *>相手商品
     READ     NFSHOMS1
              INVALID     MOVE  "INV"  TO   NFSHOMS1-INV-FLG
              NOT INVALID MOVE  SPACE  TO   NFSHOMS1-INV-FLG
     END-READ.
*
 NFSHOMS1-READ-EXIT.
     EXIT.
****************************************************************
*    本発原売価チェックファイル索引
****************************************************************
 NFGBCKF-READ-SEC            SECTION.
*
     MOVE    "NFGBCKF-READ-SEC"   TO   S-NAME.
*
     MOVE     STE-F01             TO   GBC-F01.   *>管理番号
     MOVE     STE-F05             TO   GBC-F02.   *>作場ＣＤ
     MOVE     STE-F08             TO   GBC-F03.   *>店着日
     MOVE     STE-F96             TO   GBC-F04.   *>相手商品
     MOVE     STE-F89             TO   GBC-F05.   *>原価単価
     MOVE     STE-F90             TO   GBC-F06.   *>売価単価
     READ     NFGBCKF
              INVALID     MOVE  "INV"  TO   NFGBCKF-INV-FLG
              NOT INVALID MOVE  SPACE  TO   NFGBCKF-INV-FLG
     END-READ.
*
 NFGBCKF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*件数印字
*数量訂正ファイル読込
     DISPLAY "NFGBCKL1  READ CNT = " RD-CNT            UPON CONS.
*作成カウント
     DISPLAY "NFGBCKL1  WT   CNT = " WT-CNT            UPON CONS.
*更新カウント追加
     DISPLAY "NFGBCKL1  RW   CNT = " RW-CNT           UPON CONS.
*
     CLOSE     NFSUTEL1  NFSHOMS1  NFGBCKF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
