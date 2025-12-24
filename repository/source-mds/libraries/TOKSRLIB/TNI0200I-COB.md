# TNI0200I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/TNI0200I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　基幹　　　　　　　　　            *
*    サブシステム　　　　：　受信　　　　　　　　              *
*    モジュール名　　　　：　日次更新計上エラーデータＣＳＶ指示*
*    作成日／作成者　　　：　2022/08/08 INOUE                  *
*    更新日／更新者　　　：　                                  *
*    処理概要　　　　　　：　画面より、日次更新計上エラーＣＳＶ*
*                            を出力する条件を入力し、パラメタを*
*                            出力する。　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           TNI0200I.
*                 流用:SNJ8050I
 AUTHOR.               NAV.
 DATE-WRITTEN.         2022/08/08.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*画面ファイル
     SELECT  DSPFILE   ASSIGN    TO        GS-DSPF
                       FORMAT              DSP-FMT
                       GROUP               DSP-GRP
                       PROCESSING          DSP-PRO
                       FUNCTION            DSP-FNC
                       FILE      STATUS    DSP-ST.
*取引先マスタ
*    SELECT  HTOKMS    ASSIGN    TO        TOKMS2
*                      ORGANIZATION        INDEXED
*                      ACCESS    MODE      RANDOM
*                      RECORD    KEY       TOK-F01
*                      FILE      STATUS    TOK-ST.
*日次更新計上エラーファイル
     SELECT  DNIERRL1  ASSIGN    TO        DNIERRL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       ERR-F01
                                           ERR-F02
                                           ERR-F03
                                           ERR-F04
                                           ERR-F07
                                           ERR-F08
                                           ERR-F11
                                           ERR-F12
                                           ERR-F06
                       FILE      STATUS    ERR-ST.
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
******************************************************************
****************************************************************
*    FILE = 取引先マスタ                                       *
****************************************************************
*FD  HTOKMS
*                      LABEL     RECORD    IS   STANDARD.
*                      COPY      HTOKMS    OF   XFDLIB
*                      JOINING   TOK       AS   PREFIX.
****************************************************************
*    FILE = エラーファイル                                     *
****************************************************************
 FD  DNIERRL1
                       LABEL     RECORD    IS   STANDARD.
                       COPY      DNIERRL1  OF   XFDLIB
                       JOINING   ERR       AS   PREFIX.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FNI02001  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  VLD-ST                   PIC  X(02).
     03  ERR-ST                   PIC  X(02).
     03  TOK-ST                   PIC  X(02).
     03  DSP-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
*フラグ領域
 01  FLG-AREA.
     03  READ-FLG                 PIC  X(03)  VALUE  ZERO.
     03  ERR-FLG                  PIC  9(02)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
***  存在チェック
     03  KEN-FLG                  PIC  9(01)  VALUE  ZERO.
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                      PIC  X(01)  VALUE  SPACE.
***  明細ブレイク判定
     03  WK-DENNO                 PIC  9(08)  VALUE  ZERO.
***  ラインカウント
     03  L-CNT                    PIC  9(02)  VALUE  ZERO.
***  ページカウント
     03  P-CNT                    PIC  9(03)  VALUE  ZERO.
***  伝票_１枚目判定
     03  RD-SW                    PIC  9(01)  VALUE  ZERO.
***  モード退避
     03  SAV-SHORI                PIC  9(01)  VALUE  ZERO.
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
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
*受信時間チェック
 01  WK-JIKAN.
     03  WK-HH                    PIC   9(02)  VALUE  ZERO.
     03  WK-MM                    PIC   9(02)  VALUE  ZERO.

*日付論理チェック
 01  WK-CHKDATE.
     03  WK-CHKDATE-YYYY          PIC   9(04)  VALUE  ZERO.
     03  WK-CHKDATE-MM            PIC   9(02)  VALUE  ZERO.
     03  WK-CHKDATE-DD            PIC   9(02)  VALUE  ZERO.
*
*ＰＦガイド
 01  PF-MSG-AREA.
     03  PF-MSG1.
         05  FILLER               PIC   N(15)
             VALUE NC"_取消　_終了".
     03  PF-MSG2.
         05  FILLER               PIC   N(15)
             VALUE NC"_取消　_終了　_項目戻し".
 01  PF-MSG-AREA-R       REDEFINES     PF-MSG-AREA.
     03  PF-MSG-R   OCCURS   2   PIC   N(15).
*
*メッセージの取得
 01  ERR-MSG-AREA.
     03  ERR-MSG1.
         05  FILLER              PIC   N(20)
             VALUE NC"日次更新日を入力して下さい".
     03  ERR-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"エラーファイルに登録されていません".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"無効キーです".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"日次更新日論理エラー".
     03  ERR-MSG5.
         05  FILLER              PIC   N(20)
             VALUE NC"　　　　　　　　　　　　　".
     03  ERR-MSG6.
         05  FILLER              PIC   N(20)
             VALUE NC"正しい値を入力してください".
     03  ERR-MSG7.
         05  FILLER              PIC   N(20)
             VALUE NC"　　　　　　　　　　　　　　　　".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  7   PIC   N(20).
*
 01  FILE-ERR.
     03  VLD-ERR           PIC N(20) VALUE
                        NC"ＶＬＤＦエラー".
     03  ERR-ERR           PIC N(20) VALUE
                        NC"エラーファイルエラー".
     03  TOK-ERR           PIC N(20) VALUE
                        NC"取引先マスタエラー".
     03  DSP-ERR           PIC N(20) VALUE
                        NC"画面ファイルエラー".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
*=============================================================
 LINKAGE             SECTION.
*=============================================================
   01  LINK-OUT-NDATE        PIC  9(08).
**************************************************************
**************************************************************
 PROCEDURE             DIVISION
                       USING  LINK-OUT-NDATE.
**************************************************************
 DECLARATIVES.
*TOK-ERR                   SECTION.
*    USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
*    DISPLAY     TOK-ERR   UPON      CONS.
*    DISPLAY     SEC-NAME  UPON      CONS.
*    DISPLAY     TOK-ST    UPON      CONS.
*    MOVE        "4000"    TO        PROGRAM-STATUS.
*    STOP        RUN.
 ERR-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DNIERRL1.
     DISPLAY     ERR-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPFILE.
     DISPLAY     DSP-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     DSP-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS START"     TO   S-NAME.
***
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC          UNTIL   END-FLG   =   "END".
     PERFORM   END-SEC.
***
     STOP    RUN.
 PROCESS-EXIT.
     EXIT.
****************************************************************
*             初期処理                               1.0
****************************************************************
 INIT-SEC              SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
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
     MOVE      LINK-OUT-YMD       TO   SYS-DATE.
*画面表示日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD.
*システム時間取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*ファイルのＯＰＥＮ
     OPEN     I-O       DSPFILE.
     OPEN     INPUT     DNIERRL1.
     DISPLAY "AAA" UPON CONS.
*ワークの初期化
     INITIALIZE         FLG-AREA.
*初期画面の表示
     MOVE     SPACE               TO   DSP-PRO.
     PERFORM  INIT-DSP-SEC.
*ヘッド入力へ
     MOVE    "1"                  TO   PSW.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             2.0
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*
     EVALUATE      PSW
*パラメタ入力
         WHEN      "1"  PERFORM   DSP-PARA-SEC
*確認入力
         WHEN      "2"  PERFORM   DSP-KAKU-SEC
*
         WHEN      OTHER  CONTINUE
     END-EVALUATE.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             パラメタ入力( PSW = 1 )                2.1       *
****************************************************************
 DSP-PARA-SEC         SECTION.
     MOVE     "DSP-PARA-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   PARA-CHK-SEC
                IF       ERR-FLG = ZERO
                         MOVE   "2"   TO   PSW
                END-IF
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    "4010"   TO   PROGRAM-STATUS
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
         WHEN   OTHER
                MOVE     3       TO   ERR-FLG
                GO       TO      DSP-PARA-SEC
     END-EVALUATE.
*
 DSP-PARA-EXIT.
     EXIT.
****************************************************************
*             パラメタチェック                       2.1.1     *
****************************************************************
 PARA-CHK-SEC             SECTION.
     MOVE     "PARA-CHK-SEC"     TO   S-NAME.
*
 PARA-CHK-01.
*日次更新日チェック
***  日次更新日未入力チェック
     IF       DSP-NIDATE  NOT NUMERIC
         OR   DSP-NIDATE  =  ZERO
              MOVE   1       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-NIDATE
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-NIDATE
              GO             TO   PARA-CHK-EXIT
     ELSE
***           日次更新日論理チェック
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     DSP-NIDATE     TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   MOVE  4        TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-NIDATE
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-NIDATE
                   GO             TO   PARA-CHK-EXIT
              END-IF
*
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-NIDATE
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-NIDATE
     END-IF.
*
 PARA-CHK-02.
*存在チェック1
     MOVE     SPACE      TO   ERR-REC.
     INITIALIZE               ERR-REC.
     MOVE     DSP-NIDATE TO   ERR-F01.
     START    DNIERRL1   KEY  IS >= ERR-F01 ERR-F02 ERR-F03
                                    ERR-F04 ERR-F07 ERR-F08
                                    ERR-F11 ERR-F12 ERR-F06
              INVALID
                     MOVE   2     TO   ERR-FLG
                     MOVE  "R"    TO   EDIT-OPTION  OF  DSP-NIDATE
                     MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-NIDATE
                     GO           TO   PARA-CHK-EXIT
              NOT INVALID
                     MOVE  "M"    TO   EDIT-OPTION  OF  DSP-NIDATE
                     MOVE  SPACE  TO   EDIT-CURSOR  OF  DSP-NIDATE
     END-START.
*
 PARA-CHK-03.
*存在チェック2
     READ     DNIERRL1
              AT END
                     MOVE   2     TO   ERR-FLG
                     MOVE  "R"    TO   EDIT-OPTION  OF  DSP-NIDATE
                     MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-NIDATE
                     GO           TO   PARA-CHK-EXIT
              NOT AT END
                     MOVE  "M"    TO   EDIT-OPTION  OF  DSP-NIDATE
                     MOVE  SPACE  TO   EDIT-CURSOR  OF  DSP-NIDATE
     END-READ.
*
 PARA-CHK-04.
*存在チェック3
     IF    ERR-F01  = DSP-NIDATE
           MOVE  "M"    TO   EDIT-OPTION  OF  DSP-NIDATE
           MOVE  SPACE  TO   EDIT-CURSOR  OF  DSP-NIDATE
     ELSE
           MOVE   2     TO   ERR-FLG
           MOVE  "R"    TO   EDIT-OPTION  OF  DSP-NIDATE
           MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-NIDATE
           GO           TO   PARA-CHK-EXIT
     END-IF.
*
 PARA-CHK-05.
*存在チェック4
     IF  ( ERR-F35  = "1" )  OR
         ( ERR-F36  = "1" )  OR
         ( ERR-F37  = "1" )  OR
         ( ERR-F38  = "1" )  OR
         ( ERR-F39  = "1" )  OR
         ( ERR-F40  = "1" )  OR
         ( ERR-F41  = "1" )  OR
         ( ERR-F42  = "1" )  OR
         ( ERR-F43  = "1" )  OR
         ( ERR-F44  = "1" )  OR
         ( ERR-F45  = "1" )  OR
         ( ERR-F46  = "1" )  OR
         ( ERR-F47  = "1" )  OR
         ( ERR-F48  = "1" )  OR
         ( ERR-F49  = "1" )
           MOVE  "M"    TO   EDIT-OPTION  OF  DSP-NIDATE
           MOVE  SPACE  TO   EDIT-CURSOR  OF  DSP-NIDATE
     ELSE
           GO           TO   PARA-CHK-03
     END-IF.
*
 PARA-CHK-EXIT.
     EXIT.
****************************************************************
*             確認項目入力( PSW = 2 )                2.2       *
****************************************************************
 DSP-KAKU-SEC         SECTION.
     MOVE     "DSP-KAKU-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
*               PERFORM VLD500-OUTPUT-SEC
                MOVE    "END"        TO   END-FLG
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    "4010"   TO   PROGRAM-STATUS
*項目戻し
         WHEN   "F006"
                MOVE    "1"      TO   PSW
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
         WHEN   OTHER
                MOVE     6       TO   ERR-FLG
                GO       TO      DSP-KAKU-SEC
     END-EVALUATE.
*
 DSP-KAKU-EXIT.
     EXIT.
****************************************************************
*             画面表示処理                                     *
****************************************************************
 DSP-WRITE-SEC         SECTION.
     MOVE     "DSP-WRITE-SEC"     TO   S-NAME.
*エラーメッセージセット
     IF    ERR-FLG   =    ZERO
           MOVE    SPACE              TO   DSP-MSGSPC
     ELSE
           MOVE    ERR-MSG-R(ERR-FLG) TO   DSP-MSGSPC
           MOVE    ZERO               TO   ERR-FLG
     END-IF.
*ガイドメッセージの設定
     EVALUATE   PSW
***      パラメタ項目
         WHEN   "1"
                MOVE    PF-MSG-R(1)        TO   DSP-FNCSPC
***      確認
         WHEN   "2"
                MOVE    PF-MSG-R(2)        TO   DSP-FNCSPC
***      その他
         WHEN   OTHER
                MOVE    SPACE              TO   DSP-FNCSPC
     END-EVALUATE.
*
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FNI02001"          TO   DSP-FMT.
     WRITE    DSP-FNI02001.
*
 DSP-WRITE-EXIT.
     EXIT.
****************************************************************
*             画面読込処理                                     *
****************************************************************
 DSP-READ-SEC          SECTION.
     MOVE     "DSP-READ-SEC"      TO   S-NAME.
*
     MOVE    "NE"                 TO   DSP-PRO.
*
     EVALUATE   PSW
*パラメタ項目
         WHEN   "1"
                MOVE    "PARGRP"  TO   DSP-GRP
*確認
         WHEN   "2"
                MOVE    "CHKGRP"  TO   DSP-GRP
     END-EVALUATE.
*
     MOVE    "FNI02001"           TO   DSP-FMT.
     READ    DSPFILE.
*入力項目の属性を通常にする
 DSP-READ-010.
     MOVE    SPACE                TO   DSP-PRO.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*             初期画面表示                                     *
****************************************************************
 INIT-DSP-SEC          SECTION.
     MOVE     "INIT-DSP-SEC"      TO   S-NAME.
*画面の初期化
     MOVE    SPACE                TO   DSP-FNI02001.
*システム日付転送
     MOVE    HEN-DATE             TO   DSP-SDATE.
*システム時間転送
     MOVE    HEN-TIME             TO   DSP-STIME.
*項目属性クリア　
     PERFORM      DSP-SYOKI-SEC.
*
 INT-DSP-EXIT.
     EXIT.
****************************************************************
*             画面制御項目初期化                               *
****************************************************************
 DSP-SYOKI-SEC         SECTION.
     MOVE     "INIT-DSP-SEC"      TO   S-NAME.
*
*リバース，カーソルパーク解除
***  日次更新日
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-NIDATE.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-NIDATE.
***  バッチ_（時間）
*    MOVE    "M"      TO  EDIT-OPTION  OF  DSP-BTTIME.
*    MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-BTTIME.
***  バッチ_（取引先）
*    MOVE    "M"      TO  EDIT-OPTION  OF  DSP-BTTORI.
*    MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-BTTORI.
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*             終了処理                               6.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             DNIERRL1 DSPFILE.
     MOVE    DSP-NIDATE         TO     LINK-OUT-NDATE.
**
 END-EXIT.
     EXIT.
*****************<<  TNI0200I   END PROGRAM  >>******************

```
