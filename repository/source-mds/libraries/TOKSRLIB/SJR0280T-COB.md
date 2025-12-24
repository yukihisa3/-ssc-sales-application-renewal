# SJR0280T

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SJR0280T.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受領返品取込機能構築　　　　　　　*
*    業務名　　　　　　　：　返品データ抽出　　　　　　        *
*    モジュール名　　　　：　返品データ抽出　　　　　　　      *
*    作成日／更新日　　　：　2017/08/28                        *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　受領累積データより、返品データを　*
*                            受領返品累積データの抽出する。    *
*    更新日／更新者　　　：　2019/01/27 NAV TAKAHASHI          *
*    修正概要　　　　　　：　分類（部門）連携追加              *
*    更新日／更新者　　　：　2019/08/21 NAV TAKAHASHI          *
*    修正概要　　　　　　：　コメリ伝票番号桁９桁対応下１桁削除*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SJR0280T.
 AUTHOR.                NAV TAKAHASHI.
 DATE-WRITTEN.          2017/08/28.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*商品変換テーブル
     SELECT   HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TBL-F01   TBL-F02
                        FILE STATUS    IS   TBL-STATUS.
*伝票区分変換マスタ
     SELECT   DENHENF   ASSIGN    TO        DA-01-VI-DENHENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       HEN-F01   HEN-F02
                        FILE STATUS    IS   HEN-STATUS.
*受領累積データ
     SELECT   COMRUIF   ASSIGN    TO        DA-01-VI-COMRUIL4
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       JYR-F01   JYR-F02
                                            JYR-F03   JYR-F04
                                            JYR-F32   JYR-F06
                                            JYR-F07   JYR-F08
                                            JYR-F31
                        FILE  STATUS   IS   JYR-STATUS.
*受領返品データ
     SELECT   COMRHEF   ASSIGN    TO        DA-01-VI-COMRHEL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JHR-F01   JHR-F03
                                            JHR-F02   JHR-F04
                                            JHR-F05
                        FILE  STATUS   IS   JHR-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    商品変換テーブル
******************************************************************
 FD  HSHOTBL            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
******************************************************************
*    伝票区分変換マスタ
******************************************************************
 FD  DENHENF            LABEL RECORD   IS   STANDARD.
     COPY     DENHENF   OF        XFDLIB
              JOINING   HEN       PREFIX.
******************************************************************
*    受領累積データ
******************************************************************
 FD  COMRUIF            LABEL RECORD   IS   STANDARD.
     COPY     COMRUIF   OF        XFDLIB
              JOINING   JYR       PREFIX.
******************************************************************
*    受領返品データ
******************************************************************
 FD  COMRHEF            LABEL RECORD   IS   STANDARD.
     COPY     COMRHEF   OF        XFDLIB
              JOINING   JHR       PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  IDX                     PIC  9(02)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  NG-CNT                  PIC  9(08)     VALUE  ZERO.
*
 01  WK-DENKU                PIC  X(02)     VALUE  SPACE.
 01  COMRHEF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  HSHOTBL-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  DENHENF-INV-FLG         PIC  X(03)     VALUE  SPACE.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  TBL-STATUS        PIC  X(02).
     03  HEN-STATUS        PIC  X(02).
     03  JYR-STATUS        PIC  X(02).
     03  JHR-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SJR0280T".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJR0280T".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJR0280T".
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
     03  MSG-NG.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " NG-CNT= ".
         05  CNT-NG         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-JDATE             PIC   9(08).
 01  PARA-JTIME             PIC   9(04).
 01  PARA-JTOKCD            PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE
                                       PARA-JTIME
                                       PARA-JTOKCD.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   COMRUIF.
     MOVE      "COMRUIL1"   TO   AB-FILE.
     MOVE      JYR-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HSHOTBL.
     MOVE      "SHOTBL1 "   TO   AB-FILE.
     MOVE      TBL-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DENHENF.
     MOVE      "DENHENL1"   TO   AB-FILE.
     MOVE      HEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   COMRHEF.
     MOVE      "COMRHEL1"   TO   AB-FILE.
     MOVE      JHR-STATUS   TO   AB-STS.
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
     OPEN     INPUT     HSHOTBL   DENHENF.
     OPEN     I-O       COMRUIF.
     OPEN     I-O       COMRHEF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
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
*受領累積データスタート
     MOVE      SPACE            TO     JYR-REC.
     INITIALIZE                        JYR-REC.
     MOVE      PARA-JDATE       TO     JYR-F01.
     MOVE      PARA-JTIME       TO     JYR-F02.
     MOVE      PARA-JTOKCD      TO     JYR-F03.
     START  COMRUIF  KEY  IS  >=  JYR-F01  JYR-F02  JYR-F03
                                  JYR-F04  JYR-F32  JYR-F06
                                  JYR-F07  JYR-F08  JYR-F31
            INVALID  MOVE   9   TO     END-FG
                     DISPLAY NC"＃＃対象データ無１！！"
                             UPON  CONS
                     GO         TO     INIT-EXIT
     END-START.
*受領累積データ読込
     PERFORM  COMRUIF-READ-SEC.
     IF  END-FG = 9
         DISPLAY NC"＃＃対象データ無２！！" UPON CONS
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
*返品累積データ作成
     PERFORM  EDIT-SEC.
*
     PERFORM COMRUIF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ファイル出力　　　　　　　　　　　　　　　　　　*
****************************************************************
 EDIT-SEC              SECTION.
*
     MOVE    "EDIT-SEC"     TO        S-NAME.
*
     MOVE     SPACE         TO        JHR-REC.
     INITIALIZE                       JHR-REC.
*    取引先ＣＤ
     MOVE     JYR-F03       TO        JHR-F01.
*    店舗ＣＤ
     MOVE     JYR-F07       TO        JHR-F02.
*    検収日
     MOVE     JYR-F08       TO        JHR-F03.
*    伝票番号
     MOVE     JYR-F04       TO        JHR-F04.
*#2019/08/21 NAV ST
     IF       JYR-F03 = 271934
              MOVE  JYR-F04(1:9)  TO  JHR-F04
     END-IF.
*#2019/08/21 NAV ED
*    行番号
     MOVE     JYR-F31       TO        JHR-F05.
*    伝票区分（個別）
     MOVE     JYR-F06       TO        JHR-F06.
*    検収日（サカタ）画面より入力される項目
     MOVE     JYR-F08       TO        JHR-F07.
*    ＪＡＮＣＤ
     MOVE     JYR-F33       TO        JHR-F09.
*    商品変換ＴＢＬを索引し、_番を取得する
     MOVE     JYR-F03       TO        TBL-F01.
     MOVE     JYR-F33       TO        TBL-F02.
     PERFORM HSHOTBL-READ-SEC.
     IF  HSHOTBL-INV-FLG  =  SPACE
         MOVE TBL-F08       TO        JHR-F26
         MOVE TBL-F04       TO        JHR-F08
     END-IF.
*    サカタ商品ＣＤ＋品単ＣＤ
     MOVE     JYR-F52       TO        JHR-F10.
     MOVE     JYR-F53       TO        JHR-F11.
     MOVE     JYR-F54       TO        JHR-F12.
     MOVE     JYR-F55       TO        JHR-F13.
*    商品名カナ
     MOVE     JYR-F37       TO        JHR-F14.
     MOVE     JYR-F38       TO        JHR-F15.
*    返品数量
     MOVE     JYR-F34       TO        JHR-F16.
*    原価単価
     MOVE     JYR-F35       TO        JHR-F17.
*    原価金額
     MOVE     JYR-F36       TO        JHR-F18.
*    エラーセット
     MOVE     JYR-F561      TO        JHR-F87.
     MOVE     JYR-F562      TO        JHR-F88.
*    登録情報／更新情報
     MOVE     "2920"        TO        JHR-F94.
     MOVE     "99"          TO        JHR-F95.
     MOVE     SYS-DATEW     TO        JHR-F92.
     MOVE     WK-TIME(1:6)  TO        JHR-F93.
*    受信日／受信時刻セット
     MOVE     JYR-F01       TO        JHR-F78.
     MOVE     JYR-F02       TO        JHR-F79.
*    自社伝票区分セット
     MOVE     JYR-F51       TO        JHR-F23.
*#2019/01/27 NAV ST
*    分類（部門）
     MOVE     JYR-F09       TO        JHR-F34.
*#2019/01/27 NAV ED
*
     PERFORM COMRHEF-READ-SEC.
     IF  COMRHEF-INV-FLG  =  "INV"
*        受領累積データが存在しない場合、出力する。
         WRITE  JHR-REC
*        返品累積データ作成ＦＬＧ／作成日更新
         MOVE   "1"         TO        JYR-F57
         MOVE    SYS-DATEW  TO        JYR-F58
         REWRITE JYR-REC
         ADD      1         TO        WRT-CNT
     ELSE
         ADD      1         TO        NG-CNT
     END-IF.
*
 EDIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      WRT-CNT   TO      OUT-CNT.
     MOVE      NG-CNT    TO      CNT-NG.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-NG    UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     CLOSE     COMRUIF  HSHOTBL  DENHENF  COMRHEF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　返品累積データ読込（存在チェック）
****************************************************************
 COMRHEF-READ-SEC        SECTION.
*
     MOVE   "COMRUIF-READ-SEC"  TO   S-NAME.
*
     READ  COMRHEF
           INVALID     MOVE  "INV"  TO  COMRHEF-INV-FLG
           NOT INVALID MOVE  SPACE  TO  COMRHEF-INV-FLG
     END-READ.
*
 COMRHEF-READ-EXIT.
     EXIT.
****************************************************************
*　　商品変換ＴＢＬ読込
****************************************************************
 HSHOTBL-READ-SEC        SECTION.
*
     MOVE   "HSHOTBL-READ-SEC"  TO   S-NAME.
*
     READ  HSHOTBL
           INVALID     MOVE  "INV"  TO  HSHOTBL-INV-FLG
           NOT INVALID MOVE  SPACE  TO  HSHOTBL-INV-FLG
     END-READ.
*
 HSHOTBL-READ-EXIT.
     EXIT.
****************************************************************
*　　伝票区分変換マスタ読込
****************************************************************
 DENHENF-READ-SEC        SECTION.
*
     MOVE   "DENHENF-READ-SEC"  TO   S-NAME.
*
     READ  DENHENF
           INVALID     MOVE  "INV"  TO  DENHENF-INV-FLG
           NOT INVALID MOVE  SPACE  TO  DENHENF-INV-FLG
     END-READ.
*
 DENHENF-READ-EXIT.
     EXIT.
****************************************************************
*　　受領累積データ読込
****************************************************************
 COMRUIF-READ-SEC        SECTION.
*
     MOVE   "COMRUIF-READ-SEC"  TO   S-NAME.
*
 READ001.
     READ  COMRUIF
           AT  END     MOVE  9      TO  END-FG
                       GO           TO  COMRUIF-READ-EXIT
     END-READ.
*処理件数カウント／画面表示
     ADD   1                    TO   RD-CNT.
     IF  RD-CNT(6:3)  =  "000" OR "500"
         DISPLAY "READ-CNT = " RD-CNT UPON  CONS
     END-IF.
 READ002.
*バッチ番号チェック
     IF  PARA-JDATE  =  JYR-F01
     AND PARA-JTIME  =  JYR-F02
*****AND PARA-JTOKCD =  JYR-F03
         CONTINUE
     ELSE
         MOVE  9                TO   END-FG
         GO                     TO   COMRUIF-READ-EXIT
     END-IF.
 READ002-1.
*バッチ取引先チェック
     IF  PARA-JTOKCD NOT =  ZERO
         IF  PARA-JTOKCD =  JYR-F03
             CONTINUE
         ELSE
            GO                  TO   COMRUIF-READ-SEC
         END-IF
     END-IF.
 READ003.
*伝票区分変換マスタ作成
     MOVE JYR-F03               TO   HEN-F01.
     MOVE JYR-F06               TO   HEN-F02.
     PERFORM  DENHENF-READ-SEC.
     DISPLAY "DENHENF-INV-FLG = " DENHENF-INV-FLG
     IF  DENHENF-INV-FLG = "INV"
         GO                     TO   COMRUIF-READ-SEC
     ELSE
         DISPLAY "HEN-F06 = " HEN-F06 "-" HEN-F01  "-" HEN-F02
         DISPLAY "DATA    = " JYR-F03 "-" JYR-F06
         IF HEN-F06 NOT = "1"
            GO                  TO   COMRUIF-READ-SEC
         END-IF
     END-IF.
 READ004.
*返品伝票データ作成区分が１の場合
**   IF  JYR-F57  =  "1"
**       GO                     TO   COMRUIF-READ-SEC
**   END-IF.
*
 COMRUIF-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
