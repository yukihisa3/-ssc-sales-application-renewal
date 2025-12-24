****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ新ＥＤＩシステム　　　　　　*
*    業務名　　　　　　　：　受領業務　　　　　　　　　　　　　*
*    モジュール名　　　　：　受領アンマッチリスト発行指示      *
*    作成日／更新日　　　：　12/04/04                          *
*    作成者／更新者　　　：　NAV T.TAKAHASHI                   *
*    処理概要　　　　　　：　画面より，アンマッチリストを発行す*
*                            る条件を入力する。                *
*            更新日　　　：　12/05/30                          *
*                  　　　：　受領日範囲指定を追加              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSY3719I.
 AUTHOR.               NAV.
 DATE-WRITTEN.         12/04/04.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*画面定義ファイル
     SELECT  DSPFILE   ASSIGN    TO        GS-DSPF
                       FORMAT              DSP-FMT
                       GROUP               DSP-GRP
                       PROCESSING          DSP-PRO
                       FUNCTION            DSP-FNC
                       FILE      STATUS    DSP-ST.
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FSY37191  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  DSP-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
*フラグ領域
 01  FLG-AREA.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG                  PIC  9(01)  VALUE  ZERO.
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                      PIC  X(01)  VALUE  SPACE.
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
*↓20120530
*************VALUE NC"_取消　_終了　_項目戻し".
             VALUE NC"_取消　_終了".
*↑20120530
 01  PF-MSG-AREA-R       REDEFINES     PF-MSG-AREA.
     03  PF-MSG-R   OCCURS   2   PIC   N(15).
*
*メッセージの取得
 01  ERR-MSG-AREA.
     03  ERR-MSG1.
         05  FILLER              PIC   N(20)
             VALUE NC"日付範囲を指定して下さい".
     03  ERR-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"日付論理エラーです".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"無効キーです".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"開始が終了を超えています。".
     03  ERR-MSG5.
         05  FILLER              PIC   N(20)
             VALUE NC"　".
     03  ERR-MSG6.
         05  FILLER              PIC   N(20)
             VALUE NC"　".
     03  ERR-MSG7.
         05  FILLER              PIC   N(20)
             VALUE NC"　".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  7   PIC   N(20).
*
 01  FILE-ERR.
     03  DSP-ERR           PIC N(20) VALUE
                        NC"画面ファイルエラー".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
**************************************************************
 LINKAGE               SECTION.
 01  LINK-SDATES           PIC  9(08).
 01  LINK-SDATEE           PIC  9(08).
*↓20120530
 01  LINK-JDATES           PIC  9(08).
 01  LINK-JDATEE           PIC  9(08).
*↑20120530
**************************************************************
 PROCEDURE             DIVISION      USING     LINK-SDATES
                                               LINK-SDATEE
*↓20120530
                                               LINK-JDATES
                                               LINK-JDATEE.
*↑20120530
**************************************************************
 DECLARATIVES.
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
*↓20120530
         WHEN      "2"  PERFORM   DSP-PARA2-SEC
*↑20120530
*確認入力
*↓20120530
*********WHEN      "2"  PERFORM   DSP-KAKU-SEC
         WHEN      "3"  PERFORM   DSP-KAKU-SEC
*↑20120530
*
         WHEN      OTHER  CONTINUE
     END-EVALUATE.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             納品日入力　( PSW = 1 )                2.1       *
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
                IF  ERR-FLG = ZERO
                    MOVE  "2"    TO   PSW
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
*↓20120530
****************************************************************
*             受領日入力　( PSW = 2 )                2.1       *
****************************************************************
 DSP-PARA2-SEC         SECTION.
     MOVE     "DSP-PARA2-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   PARA-CHK2-SEC
                IF  ERR-FLG = ZERO
                    MOVE  "3"    TO   PSW
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
                GO       TO      DSP-PARA2-SEC
     END-EVALUATE.
*
 DSP-PARA2-EXIT.
     EXIT.
*↑20120530
****************************************************************
*             入力納品日チェック                     2.1.1     *
****************************************************************
 PARA-CHK-SEC             SECTION.
     MOVE     "PARA-CHK-SEC"     TO   S-NAME.
*
*出荷日付開始チェック
     IF       DSP-SDATES  NOT NUMERIC
         OR   DSP-SDATES  =  ZERO
              MOVE   1       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-SDATES
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-SDATES
              GO             TO   PARA-CHK-EXIT
     ELSE
***           論理チェック
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     DSP-SDATES     TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   MOVE  2        TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-SDATES
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-SDATES
                   GO             TO   PARA-CHK-EXIT
              END-IF
*
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-SDATES
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-SDATES
     END-IF.
*出荷日付終了チェック
     IF       DSP-SDATEE  NOT NUMERIC
         OR   DSP-SDATEE  =  ZERO
              MOVE   1       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-SDATEE
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-SDATEE
              GO             TO   PARA-CHK-EXIT
     ELSE
***           論理チェック
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     DSP-SDATEE     TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   MOVE  2        TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-SDATEE
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-SDATEE
                   GO             TO   PARA-CHK-EXIT
              END-IF
*
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-SDATEE
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-SDATEE
     END-IF.
*出荷日大小チェック
     IF       DSP-SDATES  >  DSP-SDATEE
              MOVE   4       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-SDATES
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-SDATEE
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-SDATES
              GO             TO   PARA-CHK-EXIT
     ELSE
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-SDATES
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-SDATES
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-SDATEE
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-SDATEE
     END-IF.
*
 PARA-CHK-EXIT.
     EXIT.
*↓20120530
****************************************************************
*             入力受領日チェック                     2.1.1     *
****************************************************************
 PARA-CHK2-SEC             SECTION.
     MOVE     "PARA-CHK2-SEC"     TO   S-NAME.
*
*受領日開始チェック
     IF       DSP-JDATES   NOT NUMERIC
         OR   DSP-JDATES   =  ZERO
              MOVE   1       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-JDATES
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-JDATES
              GO             TO   PARA-CHK2-EXIT
     ELSE
***           論理チェック
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     DSP-JDATES     TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   MOVE  2        TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF DSP-JDATES
                   MOVE  "C"      TO   EDIT-CURSOR  OF DSP-JDATES
                   GO             TO   PARA-CHK2-EXIT
              END-IF
*
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-JDATES
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-JDATES
     END-IF.
*受領日終了チェック
     IF       DSP-JDATEE   NOT NUMERIC
         OR   DSP-JDATEE   =  ZERO
              MOVE   1       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-JDATEE
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-JDATEE
              GO             TO   PARA-CHK2-EXIT
     ELSE
***           論理チェック
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     DSP-JDATEE     TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   MOVE  2        TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF DSP-JDATEE
                   MOVE  "C"      TO   EDIT-CURSOR  OF DSP-JDATEE
                   GO             TO   PARA-CHK2-EXIT
              END-IF
*
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-JDATEE
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-JDATEE
     END-IF.
*受領日大小チェック
     IF       DSP-JDATES  >  DSP-JDATEE
              MOVE   4       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-JDATES
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-JDATEE
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-JDATES
              GO             TO   PARA-CHK2-EXIT
     ELSE
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-JDATES
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-JDATES
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-JDATEE
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-JDATEE
     END-IF.
*
 PARA-CHK2-EXIT.
     EXIT.
*↑20120530
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
                MOVE    DSP-SDATES   TO   LINK-SDATES
                MOVE    DSP-SDATEE   TO   LINK-SDATEE
*↓20120530
                MOVE    DSP-JDATES   TO   LINK-JDATES
                MOVE    DSP-JDATEE   TO   LINK-JDATEE
*↑20120530
          DISPLAY "SDATES = " LINK-SDATES UPON CONS
          DISPLAY "SDATEE = " LINK-SDATEE UPON CONS
*↓20120530
          DISPLAY "JDATES = " LINK-JDATES UPON CONS
          DISPLAY "JDATEE = " LINK-JDATEE UPON CONS
*↑20120530
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
     MOVE    "FSY37191"          TO   DSP-FMT.
     WRITE    DSP-FSY37191.
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
*納品日
         WHEN   "1"
                MOVE    "PARGRP"  TO   DSP-GRP
*↓20120530
*受領日
         WHEN   "2"
                MOVE    "JURYOU"  TO   DSP-GRP
*↑20120530
*確認
*↓20120530
*********WHEN   "2"
         WHEN   "3"
*↑20120530
                MOVE    "CHKGRP"  TO   DSP-GRP
     END-EVALUATE.
*
     MOVE    "FSY37191"           TO   DSP-FMT.
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
     MOVE    SPACE                TO   DSP-FSY37191.
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
***  日付
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SDATES.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SDATES.
*↓20120530
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-JDATES.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-JDATES.
*↑20120530
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*             終了処理                               6.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             DSPFILE.
**
 END-EXIT.
     EXIT.
*****************<<  SSY3719I   END PROGRAM  >>******************
