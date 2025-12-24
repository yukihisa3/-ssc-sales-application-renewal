# SFU3140I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SFU3140I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　社内振替　　　　　　　　　        *
*    モジュール名　　　　：　社内振替入荷予定リスト　出力指示　*
*    　　　　　　　　　　：　（出力指示）　　　　　　　　　　　*
*    作成日／作成者　　　：　2017/01/12 INOUE                  *
*    処理概要　　　　　　：　リスト発行指示　　　　　　　　　　*
*    流用　　　　　　　　：　SFU3110I                          *
*                                                              *
*    更新日／更新者　　　：　                                  *
*    修正概要　　　　　　：　                                  *
*                          　                                  *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SFU3140I.
 AUTHOR.               NAV.
 DATE-WRITTEN.         2017/01/12.
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
     SELECT  ZSOKMS1   ASSIGN    TO        ZSOKMS1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       SOK-F01
                       FILE      STATUS    SOK-ST.
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
 FD  ZSOKMS1
                       BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      ZSOKMS1   OF   XFDLIB
                       JOINING   SOK       AS   PREFIX.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FFU31401  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  SOK-ST                   PIC  X(02).
     03  DSP-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
*画面表示用ワーク
 01  DSP-WORK.
     03  WORK-PGID                PIC  X(08)  VALUE  "SFU3140I".
     03  WORK-FORMID              PIC  X(08)  VALUE  "FFU31401".
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
             VALUE NC"倉庫コードを入力してください".
     03  ERR-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"倉庫マスタに登録されていません".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"前後２年間で入力してください".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"開始が終了を超えています".
     03  ERR-MSG5.
         05  FILLER              PIC   N(20)
             VALUE NC"無効キーです".
     03  ERR-MSG6.
         05  FILLER              PIC   N(20)
             VALUE NC"正しい区分を入力して下さい".
     03  ERR-MSG7.
         05  FILLER              PIC   N(20)
             VALUE NC"入力日（日付）論理エラー".
*
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS   7  PIC   N(20).
*
 01  FILE-ERR.
     03  SOK-ERR           PIC N(20) VALUE
                        NC"倉庫マスタエラー".
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
 01  LINK-IN-SOKO        PIC  X(02).
 01  LINK-IN-DSOKO       PIC  X(02).
 01  LINK-OUT-SOKO       PIC  X(02).
 01  LINK-OUT-NYUF       PIC  9(08).
 01  LINK-OUT-NYUT       PIC  9(08).
 01  LINK-OUT-NENDOF     PIC  9(04).
 01  LINK-OUT-NENDOT     PIC  9(04).
 01  LINK-OUT-SEASONF    PIC  X(02).
 01  LINK-OUT-SEASONT    PIC  X(02).
 01  LINK-OUT-OUTKBN     PIC  X(01).
 01  LINK-OUT-PRTKBN     PIC  X(01).
*
**************************************************************
 PROCEDURE       DIVISION  USING     LINK-IN-SOKO
                                     LINK-IN-DSOKO
                                     LINK-OUT-SOKO
                                     LINK-OUT-NYUF
                                     LINK-OUT-NYUT
                                     LINK-OUT-NENDOF
                                     LINK-OUT-NENDOT
                                     LINK-OUT-SEASONF
                                     LINK-OUT-SEASONT
                                     LINK-OUT-OUTKBN
                                     LINK-OUT-PRTKBN.
**************************************************************
 DECLARATIVES.
 SOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZSOKMS1.
     DISPLAY     SOK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SOK-ST    UPON      CONS.
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
     OPEN     INPUT     ZSOKMS1.
*ワークの初期化
     INITIALIZE         FLG-AREA
                        LINK-OUT-SOKO
                        LINK-OUT-NENDOF
                        LINK-OUT-NENDOT
                        LINK-OUT-NYUF
                        LINK-OUT-NYUT
                        LINK-OUT-SEASONF
                        LINK-OUT-SEASONT
                        LINK-OUT-OUTKBN
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
*倉庫コードチェック
***  未入力時，エラー
     IF       DSP-SOKCD   =   SPACE
              MOVE     SPACE      TO   DSP-SOKCD
                     MOVE   1     TO   ERR-FLG
                     MOVE  "R"    TO   EDIT-OPTION  OF  DSP-SOKCD
                     MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-SOKCD
                     GO           TO   PARA-CHK-EXIT
     ELSE
***  倉庫マスタＲＥＡＤ
              MOVE      DSP-SOKCD     TO   SOK-F01
              READ      ZSOKMS1
              INVALID
                     MOVE   2      TO   ERR-FLG
                     MOVE  "R"     TO   EDIT-OPTION  OF  DSP-SOKCD
                     MOVE  "C"     TO   EDIT-CURSOR  OF  DSP-SOKCD
                     GO            TO   PARA-CHK-EXIT
              NOT INVALID
                     MOVE  "M"     TO   EDIT-OPTION  OF  DSP-SOKCD
                     MOVE  SPACE   TO   EDIT-CURSOR  OF  DSP-SOKCD
                     MOVE  SOK-F02 TO  DSP-SOKNM
              END-READ
     END-IF.
*
*入荷予定日（開始）チェック
***  未入力チェック
     IF       DSP-NYYDTS  NOT NUMERIC
         OR   DSP-NYYDTS  =  ZERO
              MOVE   ZERO    TO   DSP-NYYDTS
     ELSE
***  論理チェック
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     DSP-NYYDTS     TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   MOVE   7       TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-NYYDTS
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-NYYDTS
                   GO             TO   PARA-CHK-EXIT
              END-IF
*
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-NYYDTS
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-NYYDTS
     END-IF.
*
*入荷予定日（終了）チェック
***  未入力チェック
     IF       DSP-NYYDTE  NOT NUMERIC
         OR   DSP-NYYDTE  =  ZERO
         OR   DSP-NYYDTE  =  99999999
              MOVE 99999999  TO   DSP-NYYDTE
     ELSE
***  論理チェック
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     DSP-NYYDTE     TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   MOVE   7       TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-NYYDTE
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-NYYDTE
                   GO             TO   PARA-CHK-EXIT
              END-IF
*
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-NYYDTE
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-NYYDTE
     END-IF.
*
*入荷予定日　大小チェック
     IF       DSP-NYYDTS >  DSP-NYYDTE
              MOVE      4    TO   ERR-FLG
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-NYYDTS
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-NYYDTE
              MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-NYYDTS
              GO             TO   PARA-CHK-EXIT
     ELSE
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-NYYDTS
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-NYYDTE
              MOVE     SPACE TO   EDIT-CURSOR  OF  DSP-NYYDTS
     END-IF.
*
*年度（開始）チェック
***  前後２年算出
     COMPUTE  NENDO-BEFORE =  WK-CHKDATE-YYYY - 2.
     COMPUTE  NENDO-AFTER  =  WK-CHKDATE-YYYY + 2.
***  未入力チェック
     IF       DSP-NENDOS  NOT NUMERIC
         OR   DSP-NENDOS  =  ZERO
              MOVE   NENDO-BEFORE    TO   DSP-NENDOS
     ELSE
***  許容年チェック
              IF ( DSP-NENDOS  <   NENDO-BEFORE ) OR
                 ( DSP-NENDOS  >   NENDO-AFTER  )
                   MOVE   3       TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-NENDOS
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-NENDOS
                   GO             TO   PARA-CHK-EXIT
              END-IF
*
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-NENDOS
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-NENDOS
     END-IF.
*
*年度（終了）チェック
***  未入力チェック
     IF       DSP-NENDOE  NOT NUMERIC
         OR   DSP-NENDOE  =  ZERO
              MOVE NENDO-AFTER    TO   DSP-NENDOE
     ELSE
***  許容年チェック
              IF ( DSP-NENDOE  <   NENDO-BEFORE ) OR
                 ( DSP-NENDOE  >   NENDO-AFTER  )
                   MOVE   3       TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-NENDOE
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-NENDOE
                   GO             TO   PARA-CHK-EXIT
              END-IF
*
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-NENDOE
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-NENDOE
     END-IF.
*
*年度　大小チェック
     IF       DSP-NENDOS >  DSP-NENDOE
              MOVE      4    TO   ERR-FLG
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-NENDOS
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-NENDOE
              MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-NENDOS
              GO             TO   PARA-CHK-EXIT
     ELSE
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-NENDOS
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-NENDOE
              MOVE     SPACE TO   EDIT-CURSOR  OF  DSP-NENDOS
     END-IF.
*
*シーズン（開始）チェック
     IF       DSP-SEASNS  =  SPACE
              MOVE     SPACE TO   DSP-SEASNS
     END-IF.
*
*シーズン（終了）チェック
     IF       DSP-SEASNE  =  SPACE
              MOVE     "99"  TO   DSP-SEASNE
     END-IF.
*
*シーズン　大小チェック
     IF       DSP-SEASNS >  DSP-SEASNE
              MOVE      4    TO   ERR-FLG
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-SEASNS
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-SEASNE
              MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-SEASNS
              GO             TO   PARA-CHK-EXIT
     ELSE
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-SEASNS
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-SEASNE
              MOVE     SPACE TO   EDIT-CURSOR  OF  DSP-SEASNS
     END-IF.
*
*発行区分チェック
     IF    (  DSP-OUTKBN NOT =    " "  )  AND
           (  DSP-OUTKBN NOT =    "1"  )
              MOVE      6    TO   ERR-FLG
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-OUTKBN
              MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-OUTKBN
              GO             TO   PARA-CHK-EXIT
     ELSE
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-OUTKBN
              MOVE     SPACE TO   EDIT-CURSOR  OF  DSP-OUTKBN
     END-IF.
*
*出力区分チェック
     IF    (  DSP-PRTKBN NOT =    " "  )  AND
           (  DSP-PRTKBN NOT =    "1"  )
              MOVE      6    TO   ERR-FLG
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
                MOVE    DSP-SOKCD     TO   LINK-OUT-SOKO
                MOVE    DSP-NYYDTS    TO   LINK-OUT-NYUF
                MOVE    DSP-NYYDTE    TO   LINK-OUT-NYUT
                MOVE    DSP-NENDOS    TO   LINK-OUT-NENDOF
                MOVE    DSP-NENDOE    TO   LINK-OUT-NENDOT
                MOVE    DSP-SEASNS    TO   LINK-OUT-SEASONF
                MOVE    DSP-SEASNE    TO   LINK-OUT-SEASONT
                MOVE    DSP-OUTKBN    TO   LINK-OUT-OUTKBN
                MOVE    DSP-PRTKBN    TO   LINK-OUT-PRTKBN
*↓TEST
*        DISPLAY "LINK-OUT-SOKO   = " LINK-OUT-SOKO    UPON CONS
*        DISPLAY "LINK-OUT-NYUF   = " LINK-OUT-NYUF    UPON CONS
*        DISPLAY "LINK-OUT-NYUT   = " LINK-OUT-NYUT    UPON CONS
*        DISPLAY "LINK-OUT-NENDOF = " LINK-OUT-NENDOF  UPON CONS
*        DISPLAY "LINK-OUT-NENDOT = " LINK-OUT-NENDOT  UPON CONS
*        DISPLAY "LINK-OUT-SEASONF= " LINK-OUT-SEASONF UPON CONS
*        DISPLAY "LINK-OUT-SEASONT= " LINK-OUT-SEASONT UPON CONS
*        DISPLAY "LINK-OUT-OUTKBN = " LINK-OUT-OUTKBN  UPON CONS
*        DISPLAY "LINK-OUT-PRTKBN = " LINK-OUT-PRTKBN UPON CONS
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
     MOVE    "FFU31401"          TO   DSP-FMT.
     WRITE    DSP-FFU31401.
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
     MOVE    "FFU31401"           TO   DSP-FMT.
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
     MOVE    SPACE                TO   DSP-FFU31401.
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
***  倉庫コード
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SOKCD.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SOKCD.
***  入荷予定日（開始）
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-NYYDTS.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-NYYDTS.
***  入荷予定日（終了）
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-NYYDTE.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-NYYDTE.
***  年度（開始）
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-NENDOS.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-NENDOS.
***  年度（終了）
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-NENDOE.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-NENDOE.
***  シーズン（開始）
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SEASNS.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SEASNS.
***  シーズン（終了）
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SEASNE.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SEASNE.
***  発行区分
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-OUTKBN.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-OUTKBN.
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
     CLOSE             ZSOKMS1  DSPFILE.
**
 END-EXIT.
     EXIT.
*****************<<  SFU3140I   END PROGRAM  >>******************

```
