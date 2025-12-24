# SJR0430B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SJR0430B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　受領返品　　　　　　　　　        *
*    サブシステム　　　　：　返品自動計上　　　　　　　　　　　*
*    モジュール名　　　　：　返品自動計上判定処理　　　　      *
*    　　　　　　　　　　　　（通常／纏め共通）　　　　　      *
*    作成日／作成者　　　：　2021/10/26 INOUE                  *
*    処理概要　　　　　　：　対象受信日・時刻に一致する返品    *
*                            データのうち、自動計上するため　　*
*                            の情報を更新する。　　　　　　　　*
*    更新日／更新者　　　：　2022/03/17 NAV TAKAHASHI          *
*    修正概要　　　　　　：　２０分類変更　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SJR0430B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2021/10/26.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*受領返品データ
     SELECT   COMRHEF   ASSIGN    TO        DA-01-VI-COMRHEL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       JHR-F78   JHR-F79
                                            JHR-F01   JHR-F02
                                            JHR-F03   JHR-F04
                                            JHR-F05
                        FILE  STATUS   IS   JHR-STATUS.
*商品名称マスタ　
     SELECT   HMEIMS   ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F011   MEI-F0121
                                            MEI-F0122  MEI-F0123
                        FILE STATUS    IS   MEI-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    受領返品データ
******************************************************************
 FD  COMRHEF            LABEL RECORD   IS   STANDARD.
     COPY     COMRHEF   OF        XFDLIB
              JOINING   JHR       PREFIX.
******************************************************************
*    商品名称マスタ　
******************************************************************
 FD  HMEIMS             LABEL RECORD   IS   STANDARD.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  IDX                     PIC  9(02)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  REWRT-CNT               PIC  9(08)     VALUE  ZERO.
 01  REWRT-CNT2              PIC  9(08)     VALUE  ZERO.
 01  REWRT-CNT3              PIC  9(08)     VALUE  ZERO.
 01  SKIP-CNT                PIC  9(08)     VALUE  ZERO.
 01  NG-CNT                  PIC  9(08)     VALUE  ZERO.
*
 01  WK-DENKU                PIC  X(02)     VALUE  SPACE.
 01  COMRHEF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  HMEIMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  CHK-20                  PIC  X(02)     VALUE  SPACE.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  MEI-STATUS        PIC  X(02).
     03  HEN-STATUS        PIC  X(02).
     03  JHR-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SJR0430B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJR0430B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJR0430B".
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
         05  FILLER         PIC   X(09)  VALUE " UPDATE= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-NG.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " NO MEI= ".
         05  CNT-NG         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-SKIP.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " SKIP  = ".
         05  CNT-SKIP       PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
*
*チェックキー
 01  CHK-AREA.
     03  CHK-JHR-F78        PIC   9(08).
     03  CHK-JHR-F79        PIC   9(04).
     03  CHK-JHR-F01        PIC   9(08).
     03  CHK-JHR-F02        PIC   9(05).
     03  CHK-JHR-F03        PIC   9(08).
     03  CHK-JHR-F04        PIC   9(09).
*
*ブレイクキー
 01  BRK-AREA.
     03  BRK-JHR-F01        PIC   9(08).
     03  BRK-JHR-F02        PIC   9(05).
     03  BRK-JHR-F03        PIC   9(08).
     03  BRK-JHR-F04        PIC   9(09).
*
*保管キー
 01  BKP-AREA.
     03  BKP-JHR-F78        PIC   9(08).
     03  BKP-JHR-F79        PIC   9(04).
     03  BKP-JHR-F01        PIC   9(08).
     03  BKP-JHR-F02        PIC   9(05).
     03  BKP-JHR-F03        PIC   9(08).
     03  BKP-JHR-F04        PIC   9(09).
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-IN-JIKKOU         PIC   X(01).
 01  PARA-IN-BUMCD          PIC   X(04).
 01  PARA-IN-TANCD          PIC   X(02).
 01  PARA-IN-JDATE          PIC   9(08).
 01  PARA-IN-JTIME          PIC   9(04).
 01  PARA-IN-JTOKCD         PIC   9(08).
 01  PARA-OUT-INDATE        PIC   9(08).
*01  PARA-OUT-UPTIME        PIC   9(06).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-IN-JIKKOU
                                       PARA-IN-BUMCD
                                       PARA-IN-TANCD
                                       PARA-IN-JDATE
                                       PARA-IN-JTIME
                                       PARA-IN-JTOKCD
                                       PARA-OUT-INDATE.
*                                      PARA-OUT-UPTIME.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   COMRHEF.
     MOVE      "COMRHEL2"   TO   AB-FILE.
     MOVE      JHR-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HMEIMS.
     MOVE      "MEIMS1 "   TO   AB-FILE.
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
     OPEN     INPUT     HMEIMS.
     OPEN     I-O       COMRHEF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    REWRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
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
*システム時間取得
     ACCEPT    WK-TIME          FROM   TIME.
 INIT-01.
*返品累積データスタート
     MOVE      SPACE            TO     JHR-REC.
     INITIALIZE                        JHR-REC.
     MOVE      PARA-IN-JDATE    TO     JHR-F78.
     MOVE      PARA-IN-JTIME    TO     JHR-F79.
     MOVE      PARA-IN-JTOKCD   TO     JHR-F01.
     START  COMRHEF  KEY  IS  >=  JHR-F78  JHR-F79  JHR-F01
                                  JHR-F02  JHR-F03  JHR-F04
                                  JHR-F05
            INVALID  MOVE   9   TO     END-FG
                     DISPLAY NC"＃＃対象データ無１！！"
                             UPON  CONS
                     GO         TO     INIT-EXIT
     END-START.
 INIT-02.
*返品累積データ読込
     PERFORM  COMRHEL2-READ-SEC.
     IF  END-FG = 9
         DISPLAY NC"＃＃対象データ無２！！" UPON CONS
         GO      TO              INIT-EXIT
     END-IF.
*
 INIT-03.
*商品名称マスタ検索
     PERFORM  HMEIMS-READ-SEC.
     IF  HMEIMS-INV-FLG  NOT =  SPACE
*        DISPLAY NC"＃＃２０分類非該当＃＃" UPON CONS
*        DISPLAY NC"　店舗＝"    JHR-F02    UPON CONS
*        DISPLAY NC"　伝票＝"    JHR-F04    UPON CONS
*        DISPLAY NC"　行　＝"    JHR-F05    UPON CONS
*        DISPLAY NC"　商品＝"    JHR-F10    UPON CONS
*        DISPLAY NC"　品単＝"    JHR-F11 JHR-F12 JHR-F13
*                                           UPON CONS
         ADD     1               TO         NG-CNT
         GO      TO              INIT-02
     END-IF.
*
     MOVE    JHR-F78         TO   BKP-JHR-F78.
     MOVE    JHR-F79         TO   BKP-JHR-F79.
     MOVE    JHR-F01         TO   BRK-JHR-F01.
     MOVE    JHR-F02         TO   BRK-JHR-F02.
     MOVE    JHR-F03         TO   BRK-JHR-F03.
     MOVE    JHR-F04         TO   BRK-JHR-F04.
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
 MAIN-01.
*ブレイクチェック
     IF    ( JHR-F01          NOT =   BRK-JHR-F01 ) OR
           ( JHR-F02          NOT =   BRK-JHR-F02 ) OR
           ( JHR-F03          NOT =   BRK-JHR-F03 ) OR
           ( JHR-F04          NOT =   BRK-JHR-F04 )
             MOVE    JHR-F78     TO   BKP-JHR-F78
             MOVE    JHR-F79     TO   BKP-JHR-F79
             MOVE    JHR-F01     TO   BRK-JHR-F01
             MOVE    JHR-F02     TO   BRK-JHR-F02
             MOVE    JHR-F03     TO   BRK-JHR-F03
             MOVE    JHR-F04     TO   BRK-JHR-F04
     END-IF.
*
 MAIN-02.
*返品累積データ更新
     PERFORM  EDIT-SEC.
*
 MAIN-03.
*返品累積データ読込
     PERFORM  COMRHEL2-READ-SEC.
     IF  END-FG = 9
         GO   TO       MAIN-EXIT
     END-IF.
*
 MAIN-04.
*商品名称マスタ検索
     IF  END-FG = 9
         GO   TO       MAIN-EXIT
     END-IF.
     PERFORM  HMEIMS-READ-SEC.
     IF  HMEIMS-INV-FLG  NOT =  SPACE
*        DISPLAY NC"＃＃２０分類非該当＃＃" UPON CONS
*        DISPLAY NC"　店舗＝"     JHR-F02    UPON CONS
*        DISPLAY NC"　伝票＝"     JHR-F04    UPON CONS
*        DISPLAY NC"　行　＝"     JHR-F05    UPON CONS
*        DISPLAY NC"　商品＝"     JHR-F10    UPON CONS
*        DISPLAY NC"　品単＝"     JHR-F11 JHR-F12 JHR-F13
*                                            UPON CONS
         ADD     1                TO         NG-CNT
         IF    ( JHR-F01    =     BRK-JHR-F01 )   AND
               ( JHR-F02    =     BRK-JHR-F02 )   AND
               ( JHR-F03    =     BRK-JHR-F03 )   AND
               ( JHR-F04    =     BRK-JHR-F04 )
*                更新対象解除
                 PERFORM  COMRHEF-RCOV-SEC
                 MOVE     SPACE   TO  HMEIMS-INV-FLG
                 GO                  TO   MAIN-04
         ELSE
                 MOVE     JHR-F01    TO   BRK-JHR-F01
                 MOVE     JHR-F02    TO   BRK-JHR-F02
                 MOVE     JHR-F03    TO   BRK-JHR-F03
                 MOVE     JHR-F04    TO   BRK-JHR-F04
                 ADD      1          TO   SKIP-CNT
                 PERFORM  COMRHEL2-SKIP-SEC
                 GO                  TO   MAIN-04
         END-IF
         GO      TO               MAIN-03
     END-IF.
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
     MOVE      SYS-DATEW     TO      PARA-OUT-INDATE.
*    MOVE      WK-TIME(1:6)  TO      PARA-OUT-UPTIME.
*
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      NG-CNT    TO      CNT-NG.
     MOVE      SKIP-CNT  TO      CNT-SKIP.
     MOVE      REWRT-CNT TO      OUT-CNT.
*T
     DISPLAY   "RWT2="    REWRT-CNT2 UPON CONS.
     DISPLAY   "RWT3="    REWRT-CNT3 UPON CONS.
*T
     DISPLAY   MSG-IN    UPON CONS.
*    DISPLAY   MSG-NG    UPON CONS.
*    DISPLAY   MSG-SKIP  UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     CLOSE     COMRHEF  HMEIMS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　返品累積データ読込
****************************************************************
 COMRHEL2-READ-SEC        SECTION.
*
     MOVE   "COMRHEL2-READ-SEC"  TO   S-NAME.
*
 READ001.
     READ  COMRHEF  NEXT
           AT  END     MOVE  9      TO  END-FG
                       GO           TO  COMRHEL2-READ-EXIT
     END-READ.
*処理件数カウント／画面表示
     ADD   1                    TO   RD-CNT.
*T↓
*        DISPLAY "DEN      = " JHR-F04   UPON  CONS
*        DISPLAY "GYO      = " JHR-F05   UPON  CONS
*T↑
     IF  RD-CNT(6:3)  =  "000" OR "500"
         DISPLAY "READ-CNT = " RD-CNT UPON  CONS
*T↓
*        DISPLAY "F78      = " JHR-F78   UPON  CONS
*        DISPLAY "F79      = " JHR-F79   UPON  CONS
*        DISPLAY "F01      = " JHR-F01   UPON  CONS
*        DISPLAY "F02      = " JHR-F02   UPON  CONS
*        DISPLAY "F03      = " JHR-F03   UPON  CONS
*        DISPLAY "F04      = " JHR-F04   UPON  CONS
*        DISPLAY "F05      = " JHR-F05   UPON  CONS
*T↑
     END-IF.
 READ002.
*バッチ番号チェック
     IF  PARA-IN-JDATE  =  JHR-F78
     AND PARA-IN-JTIME  =  JHR-F79
         CONTINUE
     ELSE
         MOVE  9                TO   END-FG
         GO                     TO   COMRHEL2-READ-EXIT
     END-IF.
 READ003.
*バッチ取引先チェック
     IF  PARA-IN-JTOKCD NOT =  ZERO
         IF  PARA-IN-JTOKCD =  JHR-F01
             CONTINUE
         ELSE
*            GO                 TO   COMRHEL2-READ-SEC
             MOVE  9            TO   END-FG
             GO                 TO   COMRHEL2-READ-EXIT
         END-IF
     END-IF.
 READ004.
*実行区分＝" "（通常）の場合
     IF  PARA-IN-JIKKOU  = " "
         IF   JHR-F27    = " "
              CONTINUE
         ELSE
              ADD      1        TO   SKIP-CNT
              GO                TO   COMRHEL2-READ-SEC
         END-IF
     END-IF.
*
 READ005.
*実行区分＝"1"（纏め）の場合
     IF  PARA-IN-JIKKOU  = "1"
         IF   JHR-F27    = "1"
              CONTINUE
         ELSE
              ADD      1        TO   SKIP-CNT
              GO                TO   COMRHEL2-READ-SEC
         END-IF
     END-IF.
*
 READ006.
*計上日が０以外（計上済み）の場合
     IF  JHR-F86  NOT =  ZERO
              ADD      1        TO   SKIP-CNT
         GO                     TO   COMRHEL2-READ-SEC
     END-IF.
*
 COMRHEL2-READ-EXIT.
     EXIT.
****************************************************************
*　　商品名称マスタ読込
****************************************************************
 HMEIMS-READ-SEC        SECTION.
*
     MOVE  "HMEIMS-READ-SEC"  TO   S-NAME.
*
     MOVE  JHR-F10                  TO  MEI-F011.
     MOVE  JHR-F11                  TO  MEI-F0121.
     MOVE  JHR-F12                  TO  MEI-F0122.
     MOVE  JHR-F13                  TO  MEI-F0123.
     READ  HMEIMS
           INVALID     MOVE  "INV"  TO  HMEIMS-INV-FLG
                       GO           TO  HMEIMS-READ-EXIT
           NOT INVALID MOVE  SPACE  TO  HMEIMS-INV-FLG
     END-READ.
*    サカタ２０分類チェック
*****DISPLAY "MEI-F011   =  " MEI-F011    UPON  CONS.
*    DISPLAY "MEI-F0121  =  " MEI-F0121   UPON  CONS.
*    DISPLAY "MEI-F0122  =  " MEI-F0122   UPON  CONS.
*    DISPLAY "MEI-F0123  =  " MEI-F0123   UPON  CONS.
*****DISPLAY "MEI-F09    =  " MEI-F09     UPON  CONS.
     EVALUATE  MEI-F09
*#2022/03/17 NAV ST ２０分類変更伴う変更　　
         WHEN  "01"    MOVE  SPACE  TO  HMEIMS-INV-FLG
         WHEN  "02"    MOVE  SPACE  TO  HMEIMS-INV-FLG
*********WHEN  "03"    MOVE  SPACE  TO  HMEIMS-INV-FLG
*********WHEN  "04"    MOVE  SPACE  TO  HMEIMS-INV-FLG
*********WHEN  "05"    MOVE  SPACE  TO  HMEIMS-INV-FLG
*********WHEN  "06"    MOVE  SPACE  TO  HMEIMS-INV-FLG
*#2022/03/17 NAV ED ２０分類変更伴う変更　　
         WHEN  OTHER   MOVE  "INV"  TO  HMEIMS-INV-FLG
     END-EVALUATE.
*
 HMEIMS-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　返品累積データ更新　　　　　　　　　　　　　　　*
****************************************************************
 EDIT-SEC              SECTION.
*
     MOVE    "EDIT-SEC"     TO        S-NAME.
*
 EDIT-01.
*更新項目セット
*    検収日（サカタ） PENDING
     MOVE     JHR-F07       TO        JHR-F35.
     IF       JHR-F03(1:6)   =        SYS-DATEW(1:6)
              MOVE     JHR-F03        TO        JHR-F07
     ELSE
              MOVE     SYS-DATEW      TO        JHR-F07
              MOVE     0              TO        JHR-F07(7:1)
              MOVE     1              TO        JHR-F07(8:1)
     END-IF.
*
*    出荷場所
     MOVE     JHR-F08       TO        JHR-F36.
     MOVE     "01"          TO        JHR-F08.
*
*    請求区分
     IF        JHR-F01      =        "00002363"
               MOVE     "0"           TO        JHR-F22
     ELSE
               MOVE     "9"           TO        JHR-F22
     END-IF.
*
*    計上伝票区分
     MOVE     JHR-F23       TO        JHR-F37.
     MOVE     "42"          TO        JHR-F23.
*
*    計上区分
     MOVE     "1"           TO        JHR-F80.
*    入力担当者部門ＣＤ
     MOVE     "2920"        TO        JHR-F81.
*
*    入力担当者ＣＤ
     MOVE     "98"          TO        JHR-F82.
*
*    入力日
     MOVE     SYS-DATEW     TO        JHR-F83.
*
*    更新日付
     MOVE     SYS-DATEW     TO        JHR-F96.
*
*    更新時刻
     MOVE     WK-TIME(1:6)  TO        JHR-F97.
*
*    更新部門ＣＤ
     MOVE     "2920"        TO        JHR-F98.
*
*    更新担当者ＣＤ
     MOVE     "98"          TO        JHR-F99.
*
 EDIT-02.
*更新
     REWRITE  JHR-REC.
     ADD      1             TO        REWRT-CNT.
     ADD      1             TO        REWRT-CNT2.
*
*
 EDIT-EXIT.
     EXIT.
****************************************************************
*　　返品累積データ　更新対象解除
****************************************************************
 COMRHEF-RCOV-SEC        SECTION.
*
     MOVE   "COMRHEF-RCOV-SEC"  TO   S-NAME.
*
 COMRHEF-RCOV-01.
*現在キー保管
     MOVE    JHR-F01            TO   BKP-JHR-F01.
     MOVE    JHR-F02            TO   BKP-JHR-F02.
     MOVE    JHR-F03            TO   BKP-JHR-F03.
     MOVE    JHR-F04            TO   BKP-JHR-F04.
*
 COMRHEF-RCOV-02.
*返品累積データスタート
     MOVE    SPACE              TO   JHR-REC.
     INITIALIZE                      JHR-REC.
     MOVE    BKP-JHR-F78            TO   JHR-F78.
     MOVE    BKP-JHR-F79            TO   JHR-F79.
     MOVE    BRK-JHR-F01            TO   JHR-F01.
     MOVE    BRK-JHR-F02            TO   JHR-F02.
     MOVE    BRK-JHR-F03            TO   JHR-F03.
     MOVE    BRK-JHR-F04            TO   JHR-F04.
     START   COMRHEF   KEY  IS  >=  JHR-F78  JHR-F79  JHR-F01
                                    JHR-F02  JHR-F03  JHR-F04
                                    JHR-F05
*            ロジック上 INVALID は発生しない
             INVALID  DISPLAY NC"＃＃対象データ無？？"
                                                UPON  CONS
                      GO         TO     COMRHEF-RCOV-EXIT
     END-START.
*
 COMRHEF-RCOV-03.
*返品累積データ読込
     PERFORM  COMRHEL2-READ-SEC.
     IF  END-FG = 9
*        DISPLAY NC"最終データ" UPON CONS
         GO                  TO     COMRHEF-RCOV-EXIT
     END-IF.
*
 COMRHEF-RCOV-04.
         IF   ( JHR-F78      =     BKP-JHR-F78 )   AND
              ( JHR-F79      =     BKP-JHR-F79 )   AND
              ( JHR-F01      =     BRK-JHR-F01 )   AND
              ( JHR-F02      =     BRK-JHR-F02 )   AND
              ( JHR-F03      =     BRK-JHR-F03 )   AND
              ( JHR-F04      =     BRK-JHR-F04 )
            IF  JHR-F83      =     SYS-DATEW
*               更新対象解除
*                検収日（サカタ）
                 MOVE    JHR-F35        TO    JHR-F07
*                出荷場所
                 MOVE    JHR-F36        TO    JHR-F08
*                請求区分
                 MOVE    SPACE          TO    JHR-F22
*                計上伝票区分
                 MOVE    JHR-F37        TO    JHR-F23
*                計上区分（サカタ）
                 IF  PARA-IN-JIKKOU  =  " "
                     MOVE    SPACE      TO    JHR-F80
                 ELSE
                     IF      JHR-F27 =  "1"
                             MOVE       "1"   TO    JHR-F80
                     END-IF
                 END-IF
*                入力担当者部門CD
                 MOVE    SPACE          TO    JHR-F81
*                入力担当者CD
                 MOVE    SPACE          TO    JHR-F82
*                入力日
                 MOVE    ZERO           TO    JHR-F83
*                更新日付
                 MOVE    ZERO           TO    JHR-F96
*                更新時刻
                 MOVE    ZERO           TO    JHR-F97
*                更新部門CD
                 MOVE    SPACE          TO    JHR-F98
*                更新担当者CD
                 MOVE    SPACE          TO    JHR-F99
*
                 REWRITE                      JHR-REC
                 COMPUTE RD-CNT     =   RD-CNT     -  1
                 COMPUTE REWRT-CNT  =   REWRT-CNT  -  1
                 ADD        1           TO    REWRT-CNT3
                 GO                     TO    COMRHEF-RCOV-03
            ELSE
                 GO                     TO    COMRHEF-RCOV-03
            END-IF
         ELSE
                 GO                     TO    COMRHEF-RCOV-EXIT
         END-IF.
*
 COMRHEF-RCOV-EXIT.
     EXIT.
****************************************************************
*　　返品累積データ　更新対象外レコード読み飛ばし
****************************************************************
 COMRHEL2-SKIP-SEC        SECTION.
*
     MOVE   "COMRHEL2-SKIP-SEC"  TO   S-NAME.
*
 COMRHEL2-SKIP-01.
*返品累積データ読込
     PERFORM  COMRHEL2-READ-SEC.
     IF  END-FG = 9
*        DISPLAY NC"最終データ" UPON CONS
         GO                  TO     COMRHEL2-SKIP-EXIT
     END-IF.
*
 COMRHEL2-SKIP-02.
         IF    ( JHR-F78    =     BKP-JHR-F78 )   AND
               ( JHR-F79    =     BKP-JHR-F79 )   AND
               ( JHR-F01    =     BRK-JHR-F01 )   AND
               ( JHR-F02    =     BRK-JHR-F02 )   AND
               ( JHR-F03    =     BRK-JHR-F03 )   AND
               ( JHR-F04    =     BRK-JHR-F04 )
                 ADD   1    TO    SKIP-CNT
                 GO         TO    COMRHEL2-SKIP-01
         ELSE
                 GO         TO    COMRHEL2-SKIP-EXIT
         END-IF.
*
 COMRHEL2-SKIP-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
