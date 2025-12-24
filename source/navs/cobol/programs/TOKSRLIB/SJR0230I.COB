****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　受領返品　　　　　　　　　        *
*    モジュール名　　　　：　受信エラーリスト再発行指示　　　　*
*    　　　　　　　　　　：　（出力指示）　　　　　　　　　　　*
*    作成日／作成者　　　：　2017/09/15 INOUE                  *
*    処理概要　　　　　　：　リスト発行指示　　　　　　　　　　*
*    流用　　　　　　　　：　SFU3110I                          *
*                                                              *
*    更新日／更新者　　　：　                                  *
*    修正概要　　　　　　：　                                  *
*                          　                                  *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SJR0230I.
 AUTHOR.               NAV.
 DATE-WRITTEN.         2017/09/15.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*倉庫マスタ
*    SELECT  ZSOKMS1   ASSIGN    TO        ZSOKMS1
*                      ORGANIZATION        INDEXED
*                      ACCESS    MODE      RANDOM
*                      RECORD    KEY       SOK-F01
*                      FILE      STATUS    SOK-ST.
*画面定義ファイル
     SELECT  DSPFILE   ASSIGN    TO        GS-DSPF
                       FORMAT              DSP-FMT
                       GROUP               DSP-GRP
                       PROCESSING          DSP-PRO
                       FUNCTION            DSP-FNC
                       FILE      STATUS    DSP-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*  FILE= 倉庫マスタ                                          *
****************************************************************
*FD  ZSOKMS1
*                      BLOCK     CONTAINS  8    RECORDS
*                      LABEL     RECORD    IS   STANDARD.
*                      COPY      ZSOKMS1   OF   XFDLIB
*                      JOINING   SOK       AS   PREFIX.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FJR02301  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
*    03  SOK-ST                   PIC  X(02).
     03  DSP-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
*画面表示用ワーク
 01  DSP-WORK.
     03  WORK-PGID                PIC  X(08)  VALUE  "SJR0230I".
     03  WORK-FORMID              PIC  X(08)  VALUE  "FJR02301".
*フラグ領域
 01  FLG-AREA.
     03  READ-FLG                 PIC  9(01)  VALUE  ZERO.
     03  ERR-FLG                  PIC  9(02)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  SOKKBN-FLG               PIC  9(01)  VALUE  ZERO.
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                      PIC  X(01)  VALUE  SPACE.
***  モード退避
     03  SAV-SHORI                PIC  9(01)  VALUE  ZERO.
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(05)     VALUE " *** ".
     03  S-NAME                   PIC  X(30).
*
*システム日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-DATE                  PIC  9(06)  VALUE  ZERO.
     03  SYS-DATE                 PIC  9(08).
*
*受信時間チェック
 01  WK-JIKAN.
     03  WK-HH                    PIC   9(02)  VALUE  ZERO.
     03  WK-MM                    PIC   9(02)  VALUE  ZERO.
*
*日付論理チェック
 01  WK-CHKDATE.
     03  WK-CHKDATE-YYYY          PIC   9(04)  VALUE  ZERO.
     03  WK-CHKDATE-MM            PIC   9(02)  VALUE  ZERO.
     03  WK-CHKDATE-DD            PIC   9(02)  VALUE  ZERO.
*前後２年度
 01  NENDO-BEFORE                 PIC   9(04)  VALUE  ZERO.
 01  NENDO-AFTER                  PIC   9(04)  VALUE  ZERO.
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
             VALUE NC"正しい日付を入力して下さい".
     03  ERR-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"正しい時刻を入力して下さい".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"　　　　　　　　　　　　　　　　　　　　".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"　　　　　　　　　　　　　　　　　　　　".
     03  ERR-MSG5.
         05  FILLER              PIC   N(20)
             VALUE NC"無効キーです".
     03  ERR-MSG6.
         05  FILLER              PIC   N(20)
             VALUE NC"正しい区分を入力して下さい".
*
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS   6  PIC   N(20).
*
 01  FILE-ERR.
*    03  SOK-ERR           PIC N(20) VALUE
*                       NC"倉庫マスタエラー".
     03  DSP-ERR           PIC N(20) VALUE
                        NC"画面ファイルエラー".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
******************************************************************
 LINKAGE           SECTION.
******************************************************************
 01  LINK-OUT-JDATE      PIC  9(08).
 01  LINK-OUT-JTIME      PIC  9(04).
 01  LINK-OUT-PRTKBN     PIC  X(01).
*
**************************************************************
 PROCEDURE       DIVISION  USING     LINK-OUT-JDATE
                                     LINK-OUT-JTIME
                                     LINK-OUT-PRTKBN.
**************************************************************
 DECLARATIVES.
*SOK-ERR                   SECTION.
*    USE         AFTER     EXCEPTION PROCEDURE ZSOKMS1.
*    DISPLAY     SOK-ERR   UPON      CONS.
*    DISPLAY     SEC-NAME  UPON      CONS.
*    DISPLAY     SOK-ST    UPON      CONS.
*    MOVE        "4000"    TO        PROGRAM-STATUS.
*    STOP        RUN.
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
 CONTROL-EXIT.
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
     MOVE      LINK-OUT-YMD(1:4)  TO   WK-CHKDATE-YYYY.
     ACCEPT    WK-TIME          FROM   TIME.
*ファイルのＯＰＥＮ
     OPEN     I-O       DSPFILE.
*    OPEN     INPUT     ZSOKMS1.
*ワークの初期化
     INITIALIZE         FLG-AREA
                        LINK-OUT-JDATE
                        LINK-OUT-JTIME
                        LINK-OUT-PRTKBN.
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
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    "4010"   TO   PROGRAM-STATUS
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
         WHEN   OTHER
                MOVE     5       TO   ERR-FLG
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
*受信日チェック
***  未入力時，エラー
     IF     ( DSP-JDATE    NOT  NUMERIC )  OR
            ( DSP-JDATE    =    ZERO    )
              MOVE   1     TO   ERR-FLG
              MOVE  "R"    TO   EDIT-OPTION  OF  DSP-JDATE
              MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-JDATE
              GO           TO   PARA-CHK-EXIT
     ELSE
***  論理チェック
              MOVE     "2"            TO      LINK-IN-KBN
              MOVE     ZERO           TO      LINK-IN-YMD6
              MOVE     DSP-JDATE      TO      LINK-IN-YMD8
              MOVE     ZERO           TO      LINK-OUT-RET
              MOVE     ZERO           TO      LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   MOVE   1     TO   ERR-FLG
                   MOVE  "R"    TO   EDIT-OPTION  OF  DSP-JDATE
                   MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-JDATE
                   GO           TO   PARA-CHK-EXIT
              ELSE
                   MOVE  " "    TO   EDIT-OPTION  OF  DSP-JDATE
                   MOVE  " "    TO   EDIT-CURSOR  OF  DSP-JDATE
              END-IF
     END-IF.
*
*受信時刻チェック
     IF     ( DSP-JTIME   NOT NUMERIC )  OR
            ( DSP-JTIME   =   ZERO    )
              IF     ERR-FLG  =    ZERO
                     MOVE     2    TO    ERR-FLG
              END-IF
              MOVE  "R"    TO   EDIT-OPTION  OF  DSP-JTIME
              MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-JTIME
              GO           TO   PARA-CHK-EXIT
     ELSE
***           バッチ_（時間）論理チェック
              MOVE     DSP-JTIME     TO  WK-JIKAN
              IF       WK-HH  <=  ZERO  OR  WK-HH  >  24 OR
                       WK-MM  <   ZERO  OR  WK-MM  >  59
                       IF     ERR-FLG   =   ZERO
                              MOVE      2   TO       ERR-FLG
                       END-IF
                       MOVE "R"    TO EDIT-OPTION OF DSP-JTIME
                       MOVE "C"    TO EDIT-CURSOR OF DSP-JTIME
                       GO          TO PARA-CHK-EXIT
              ELSE
                       MOVE "M"    TO EDIT-OPTION OF DSP-JTIME
                       MOVE SPACE  TO EDIT-CURSOR OF DSP-JTIME
              END-IF
     END-IF.
*
*出力区分チェック
     IF    (  DSP-PRTKBN NOT =    " "  )  AND
           (  DSP-PRTKBN NOT =    "1"  )
              IF     ERR-FLG =    ZERO
                     MOVE    6    TO    ERR-FLG
              END-IF
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-PRTKBN
              MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-PRTKBN
              GO             TO   PARA-CHK-EXIT
     ELSE
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-PRTKBN
              MOVE     SPACE TO   EDIT-CURSOR  OF  DSP-PRTKBN
     END-IF.
*
     MOVE    "2"       TO    PSW.
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
                MOVE    DSP-JDATE     TO   LINK-OUT-JDATE
                MOVE    DSP-JTIME     TO   LINK-OUT-JTIME
                MOVE    DSP-PRTKBN    TO   LINK-OUT-PRTKBN
*↓TEST
*        DISPLAY "LINK-OUT-JDATE  = " LINK-OUT-JDATE   UPON CONS
*        DISPLAY "LINK-OUT-JTIME  = " LINK-OUT-JTIME   UPON CONS
*        DISPLAY "LINK-OUT-PRTKBN = " LINK-OUT-PRTKBN  UPON CONS
*↑TEST
                MOVE    "END"         TO   END-FLG
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
                MOVE     1       TO   ERR-FLG
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
     MOVE    "FJR02301"          TO   DSP-FMT.
     WRITE    DSP-FJR02301.
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
                MOVE    "GRP001"  TO   DSP-GRP
*確認
         WHEN   "2"
                MOVE    "ENDCHK"  TO   DSP-GRP
     END-EVALUATE.
*
     MOVE    "FJR02301"           TO   DSP-FMT.
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
     MOVE    SPACE                TO   DSP-FJR02301.
*プログラムＩＤ転送
     MOVE    WORK-PGID            TO   DSP-PGID.
*ＦＯＲＭＩＤ転送
     MOVE    WORK-FORMID          TO   DSP-FORMID.
*システム日付転送
     MOVE    SYS-DATE             TO   DSP-SDATE.
*システム時間転送
     MOVE    WK-TIME(1:6)         TO   DSP-STIME.
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
***  受信日
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-JDATE.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-JDATE.
***  受信時刻
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-JTIME.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-JTIME.
***  出力区分
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-PRTKBN.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-PRTKBN.
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             DSPFILE.
**
 END-EXIT.
     EXIT.
*****************<<  SJR0230I   END PROGRAM  >>******************
