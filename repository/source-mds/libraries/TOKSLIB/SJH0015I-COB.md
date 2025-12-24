# SJH0015I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SJH0015I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受配信管理システム　　　　　　　　*
*    業務名　　　　　　　：　ＥＯＳ管理　　　　　　　　        *
*    モジュール名　　　　：　受信件数照会                      *
*    作成日／作成者　　　：　1999/10/04  /HAGIWARA             *
*    更新日／更新者　　　：　                                  *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SJH0015I.
 AUTHOR.               HAGIWARA.
 DATE-WRITTEN.         99/10/04.
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
*出荷場所別件数ファイル
     SELECT  JHMKENF   ASSIGN    TO        JHMKENL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       KEN-F01
                                           KEN-F02
                                           KEN-F03
                                           KEN-F04
                       FILE      STATUS    KEN-ST.
*取引先マスタ
     SELECT  HTOKMS    ASSIGN    TO        TOKMS2
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TOK-F01
                       FILE      STATUS    TOK-ST.
*倉庫マスタ
     SELECT  ZSOKMS    ASSIGN    TO        ZSOKMS1
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
*    FILE = 出荷場所別件数Ｆ                                   *
****************************************************************
 FD  JHMKENF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JHMKENF   OF   XFDLIB
                       JOINING   KEN       AS   PREFIX.
****************************************************************
*    FILE = 取引先マスタ                                       *
****************************************************************
 FD  HTOKMS
                       BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTOKMS    OF   XFDLIB
                       JOINING   TOK       AS   PREFIX.
****************************************************************
*  FILE= 倉庫マスタ                                          *
****************************************************************
 FD  ZSOKMS
                       BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      ZSOKMS    OF   XFDLIB
                       JOINING   SOK       AS   PREFIX.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FJH00151  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  KEN-ST                   PIC  X(02).
     03  TOK-ST                   PIC  X(02).
     03  DSP-ST                   PIC  X(02).
     03  SOK-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
*フラグ領域
 01  FLG-AREA.
     03  ERR-FLG                  PIC  9(01)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  RD-FLG                   PIC  X(03)  VALUE  SPACE.
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                      PIC  X(01)  VALUE  SPACE.
*
     03  IX                       PIC  9(02)  VALUE  ZERO.
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
 01  WK-JIKAN.
     03  WK-HH                    PIC  9(02)  VALUE  ZERO.
     03  WK-MM                    PIC  9(02)  VALUE  ZERO.
*
*ＰＦガイド
 01  PF-MSG-AREA.
     03  PF-MSG1.
         05  FILLER               PIC   N(20)
      VALUE NC"_取消_終了　　　　　　　　　　　".
*    03  PF-MSG2.
*        05  FILLER               PIC   N(20)
*     VALUE NC"_取消_終了_前頁_次頁".
*01  PF-MSG-AREA-R       REDEFINES     PF-MSG-AREA.
*    03  PF-MSG-R   OCCURS   2   PIC   N(20).
*
*メッセージの取得
 01  ERR-MSG-AREA.
     03  ERR-MSG1.
         05  FILLER              PIC   N(20)
             VALUE NC"バッチ_を正しく入力して下さい".
     03  ERR-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"キーが無効です".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"バッチ_（日付）論理エラー".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"バッチ_（時間）論理エラー".
     03  ERR-MSG5.
         05  FILLER              PIC   N(20)
             VALUE NC"取引先マスタに存在しません".
     03  ERR-MSG6.
         05  FILLER              PIC   N(20)
             VALUE NC"入力したバッチ_のデータは存在しません".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  6   PIC   N(20).
*基本スケジュール退避エリア
 01  TABLE-AREA.
     03  TBL-JDATE1               PIC   9(04).
     03  TBL-JDATE2               PIC   9(02).
     03  TBL-JDATE3               PIC   9(02).
     03  TABLE1      OCCURS  3.
         05  TABLE2  OCCURS  16.
             07  TBL-MTIME        PIC   9(04).
             07  TBL-MTOKCD       PIC   9(08).
             07  TBL-MJTMS        PIC   9(04).
             07  TBL-MJTME        PIC   9(04).
             07  TBL-MHTMS        PIC   9(04).
             07  TBL-MHTME        PIC   9(04).
             07  TBL-MJKEN        PIC   9(05).
             07  TBL-MDKEN        PIC   9(05).
             07  TBL-MKEKA        PIC   9(02).
*照会日付退避ワーク
 01  GAMEN-DATE.
     03  GAMEN-YY                 PIC   9(04)  VALUE  ZERO.
     03  GAMEN-MM                 PIC   9(02)  VALUE  ZERO.
     03  GAMEN-DD                 PIC   9(02)  VALUE  ZERO.
*
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  KEN-ERR           PIC N(15) VALUE
                        NC"出荷場所別件数Ｆエラー".
     03  TOK-ERR           PIC N(15) VALUE
                        NC"取引先マスタエラー".
     03  SOK-ERR           PIC N(15) VALUE
                        NC"倉庫マスタエラー".
     03  DSP-ERR           PIC N(15) VALUE
                        NC"画面ファイルエラー".
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
***  エラーファイル名
 01  ERR-FILE.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-FILE   => ".
     03  E-FILE                   PIC  X(08).
***  エラーステータス名
 01  ERR-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-STATUS => ".
     03  E-ST                     PIC  9(02).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
**************************************************************
 PROCEDURE             DIVISION.
**************************************************************
 DECLARATIVES.
 KEN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JHMKENF.
     MOVE        KEN-ST    TO        E-ST.
     MOVE        "JHMKENF" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     KEN-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     MOVE        TOK-ST    TO        E-ST.
     MOVE        "HTOKMS"  TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     TOK-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZSOKMS.
     MOVE        SOK-ST    TO        E-ST.
     MOVE        "ZSOKMS"  TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     SOK-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPFILE.
     MOVE        DSP-ST    TO        E-ST.
     MOVE        "DSPFILE" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     DSP-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
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
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
*画面表示日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD.
*システム時刻取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*ファイルのＯＰＥＮ
     OPEN      INPUT            JHMKENF.
     OPEN      I-O              DSPFILE.
     OPEN      INPUT            HTOKMS  ZSOKMS.
*ワークの初期化
     INITIALIZE                 FLG-AREA.
*初期画面の表示
     MOVE     SPACE               TO   DSP-PRO.
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
*処理区分入力(2.1)
         WHEN      "1"       PERFORM   DSP-INIT-SEC
*バッチ_入力(2.2)
         WHEN      "2"       PERFORM   DSP-HEAD-SEC
*確認入力    (2.3)
         WHEN      "3"       PERFORM   DSP-KAKU-SEC
*以外
         WHEN      OTHER     CONTINUE
     END-EVALUATE.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE     "END-SEC"          TO   S-NAME.
*ファイル ＣＬＯＳＥ
     CLOSE             DSPFILE   JHMKENF   HTOKMS   ZSOKMS.
**
 END-EXIT.
     EXIT.
****************************************************************
*             初期画面表示( PSW = 1 )                2.1       *
****************************************************************
 DSP-INIT-SEC          SECTION.
     MOVE     "DSP-INIT-SEC"      TO   S-NAME.
*初期画面の表示
     MOVE     SPACE               TO   DSP-PRO.
*画面の初期化
     MOVE    SPACE                TO   DSP-FJH00151.
*ＦＯＲＭ－ＩＤセット
     MOVE    "FJH00151"           TO   DSP-FORMID.
*ＰＲＯＧＲＡＭ－ＩＤセット
     MOVE    "SJH0015I"           TO   DSP-PGID.
*システム日付転送
     MOVE    HEN-DATE             TO   DSP-SDATE.
*システム時間転送
     MOVE    HEN-TIME             TO   DSP-STIME.
*項目属性クリア
     PERFORM  DSP-SYOKI-SEC.
*バッチ_入力へ
     MOVE    "2"                  TO   PSW.
*
 DSP-INIT-EXIT.
     EXIT.
****************************************************************
*             バッチ_入力( PSW = 2 )                2.2       *
****************************************************************
 DSP-HEAD-SEC          SECTION.
     MOVE     "DSP-HEAD-SEC"      TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   HEAD-CHK-SEC
                IF        ERR-FLG  =  ZERO
                          MOVE  "3"   TO   PSW
                          PERFORM     BODY-WRITE-SEC
                END-IF
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
         WHEN   OTHER
                MOVE     2       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-HEAD-EXIT.
     EXIT.
****************************************************************
*             ヘッダチェック                         2.2.1     *
****************************************************************
 HEAD-CHK-SEC             SECTION.
     MOVE     "HEAD-CHK-SEC"     TO   S-NAME.
*バッチ_（日付）チェック
***  バッチ_（日付）未入力チェック
     IF       DSP-BTDATE  NOT NUMERIC OR
              DSP-BTDATE  =  ZERO
              MOVE   1       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-BTDATE
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-BTDATE
              GO             TO   HEAD-CHK-EXIT
     ELSE
***           バッチ_（日付）論理チェック
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     DSP-BTDATE     TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   NOT = ZERO
                   MOVE  3        TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-BTDATE
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-BTDATE
                   GO             TO   HEAD-CHK-EXIT
              END-IF
*
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-BTDATE
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-BTDATE
     END-IF.
*
*バッチ_（時間）チェック
***  バッチ_（時間）未入力チェック
     IF       DSP-BTTIME  NUMERIC AND
              DSP-BTTIME  NOT  = ZERO
***           バッチ_（時間）論理チェック
              MOVE     DSP-BTTIME    TO  WK-JIKAN
              IF       WK-HH  <=  ZERO  OR  WK-HH  >  24 OR
                       WK-MM  <   ZERO  OR  WK-MM  >  59
                       IF     ERR-FLG  =  ZERO
                              MOVE    4    TO    ERR-FLG
                       END-IF
                       MOVE "R"    TO EDIT-OPTION OF DSP-BTTIME
                       MOVE "C"    TO EDIT-CURSOR OF DSP-BTTIME
                       GO          TO HEAD-CHK-EXIT
              ELSE
                       MOVE "M"    TO EDIT-OPTION OF DSP-BTTIME
                       MOVE SPACE  TO EDIT-CURSOR OF DSP-BTTIME
              END-IF
     ELSE
***           ゼロは入力可とする
              IF   DSP-BTTIME  NOT NUMERIC OR
                   DSP-BTTIME  NOT =  ZERO
                   MOVE   1       TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-BTTIME
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-BTTIME
                   GO             TO   HEAD-CHK-EXIT
              ELSE
                   MOVE  "M"      TO   EDIT-OPTION OF DSP-BTTIME
                   MOVE  SPACE    TO   EDIT-CURSOR OF DSP-BTTIME
              END-IF
     END-IF.
*
*バッチ_（取引先）チェック
***  バッチ_（取引先）未入力チェック
     IF       DSP-BTTORI  NOT NUMERIC OR
              DSP-BTTORI  =  ZERO
              MOVE   1       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-BTTORI
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-BTTORI
              GO             TO   HEAD-CHK-EXIT
     ELSE
***           取引先ＲＥＡＤ
              MOVE     DSP-BTTORI TO   TOK-F01
              READ     HTOKMS
              INVALID
                     MOVE   5     TO   ERR-FLG
                     MOVE  "R"    TO   EDIT-OPTION  OF  DSP-BTTORI
                     MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-BTTORI
                     MOVE   SPACE TO   DSP-TORINM
                     GO           TO   HEAD-CHK-EXIT
              NOT INVALID
                     MOVE  "M"    TO   EDIT-OPTION  OF  DSP-BTTORI
                     MOVE  SPACE  TO   EDIT-CURSOR  OF  DSP-BTTORI
                     MOVE  TOK-F03  TO DSP-TORINM
              END-READ
     END-IF.
*出荷場所別件数ファイル存在チェック
     MOVE     DSP-BTDATE     TO   KEN-F01.
     MOVE     DSP-BTTIME     TO   KEN-F02.
     MOVE     DSP-BTTORI     TO   KEN-F03.
     START    JHMKENF   KEY  IS    =   KEN-F01
                                       KEN-F02
                                       KEN-F03
     INVALID
              MOVE      6    TO   ERR-FLG
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-BTDATE
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-BTTIME
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-BTTORI
              MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-BTDATE
              GO             TO   HEAD-CHK-EXIT
     NOT INVALID
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-BTDATE
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-BTTIME
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-BTTORI
              MOVE     SPACE TO   EDIT-CURSOR  OF  DSP-BTDATE
     END-START.
*出荷場所別件数Ｆ，１件目読込み
     PERFORM  JHMKENF-READ-SEC.
 HEAD-CHK-EXIT.
     EXIT.
****************************************************************
*             明細出力処理　　                                 *
****************************************************************
 BODY-WRITE-SEC          SECTION.
     MOVE     "WORK-DSP-SEC"      TO   S-NAME.
*
*    出荷場所別件数Ｆを読みながら明細項目をセット
     PERFORM  VARYING IX FROM 1 BY 1 UNTIL IX > 14
                                     OR  RD-FLG = "END"
***     出荷場所
        MOVE     KEN-F04          TO   DSP-SYUKCD(IX)
***     出荷場所名称
        MOVE     KEN-F04          TO   SOK-F01
        READ     ZSOKMS
        INVALID
            MOVE   SPACE     TO   DSP-SYUKNM(IX)
            NOT INVALID
            MOVE  SOK-F02    TO   DSP-SYUKNM(IX)
        END-READ
***     受信時間
        MOVE     KEN-F07          TO   DSP-JUSINT(IX)
***     伝票発行
        IF       KEN-F08  =  1111
                 MOVE  NC"完了"   TO   DSP-DENHAK(IX)
        ELSE
                 MOVE  SPACE      TO   DSP-DENHAK(IX)
        END-IF
********MOVE     KEN-F08          TO   DSP-DENHAK(IX)
***     発注集計
        IF       KEN-F09  =  1111
                 MOVE  NC"完了"   TO   DSP-HACYU(IX)
        ELSE
                 MOVE  SPACE      TO   DSP-HACYU(IX)
        END-IF
********MOVE     KEN-F09          TO   DSP-HACYU(IX)
***     受信件数
        MOVE     KEN-F10          TO   DSP-MJKEN(IX)
***     伝票枚数
        MOVE     KEN-F11          TO   DSP-MDKEN(IX)
***
        PERFORM  JHMKENF-READ-SEC
     END-PERFORM.
*
 BODY-WRITE-EXIT.
     EXIT.
****************************************************************
*             確認処理　入力（ PSW = 3 ）            2.3
****************************************************************
 DSP-KAKU-SEC          SECTION.
     MOVE     "DSP-KAKU-SEC"      TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                MOVE    "3"      TO   PSW
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
***             項目初期化
                MOVE    SPACE    TO   RD-FLG
                MOVE    ZERO     TO   IX
***
                CLOSE   JHMKENF
                OPEN    INPUT    JHMKENF
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
         WHEN   OTHER
                MOVE     2       TO   ERR-FLG
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
           MOVE    SPACE              TO   DSP-ERRMSG
     ELSE
           MOVE    ERR-MSG-R(ERR-FLG) TO   DSP-ERRMSG
*          MOVE    ZERO               TO   ERR-FLG
     END-IF.
*ガイドメッセージの設定
     MOVE    PF-MSG1             TO   DSP-PFGAID.
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FJH00151"          TO   DSP-FMT.
     WRITE    DSP-FJH00151.
     IF    ERR-FLG = ZERO
           PERFORM  DSP-SYOKI-SEC
     END-IF.
     MOVE     ZERO               TO   ERR-FLG.
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
*バッチ_
         WHEN   "2"
                MOVE    "HEAD"    TO   DSP-GRP
*確認
         WHEN   "3"
                MOVE    "KAKU"    TO   DSP-GRP
     END-EVALUATE.

     MOVE    "FJH00151"           TO   DSP-FMT.
     READ    DSPFILE.
*入力項目の属性を通常にする
 DSP-READ-010.
*    MOVE    ZERO                 TO   ERR-FLG.
     MOVE    SPACE                TO   DSP-PRO.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*             画面制御項目初期化                               *
****************************************************************
 DSP-SYOKI-SEC         SECTION.
     MOVE     "DSP-SYOKI-SEC"    TO   S-NAME.
*リバース，カーソルパーク解除
***  バッチ_
     MOVE "M"   TO EDIT-OPTION OF DSP-BTDATE.
     MOVE "M"   TO EDIT-OPTION OF DSP-BTTIME.
     MOVE "M"   TO EDIT-OPTION OF DSP-BTTORI.
     MOVE SPACE TO EDIT-CURSOR OF DSP-BTDATE.
     MOVE SPACE TO EDIT-CURSOR OF DSP-BTTIME.
     MOVE SPACE TO EDIT-CURSOR OF DSP-BTTORI.
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*             出荷場所別件数Ｆ読込み　　　                     *
****************************************************************
 JHMKENF-READ-SEC      SECTION.
     MOVE     "JHMKENF-READ-SEC"  TO   S-NAME.
*マスタ読込み
     READ JHMKENF NEXT AT END
          MOVE   "END"    TO   RD-FLG
     END-READ.
*画面バッチ_以外の時、終了へ
     IF      KEN-F01  NOT = DSP-BTDATE
        OR   KEN-F02  NOT = DSP-BTTIME
        OR   KEN-F03  NOT = DSP-BTTORI
             MOVE     "END"       TO   RD-FLG
     END-IF.
*
 JHMKENF-READ-EXIT.
     EXIT.
*******************< PROGRAM-END SJH0015I >*********************

```
