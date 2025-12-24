****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　基幹システム改善　　　　　　　　　*
*    業務名　　　　　　　：　日時更新条件マスタ　　　　        *
*    モジュール名　　　　：　日時更新条件マスタ保守　　　　    *
*    作成日／作成者　　　：　2005/12/15   NAV                  *
*    更新日／更新者　　　：　                                  *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SJH0100I.
 AUTHOR.               NAV.
 DATE-WRITTEN.         05/12/15.
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
*日時更新条件マスタ　　　
     SELECT  JHMNITF   ASSIGN    TO        JHMNITL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       NIT-F01
                       FILE      STATUS    NIT-ST.
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
*    FILE = 日時更新条件マスタ　　                             *
****************************************************************
 FD  JHMNITF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JHMNITF   OF   XFDLIB
                       JOINING   NIT       AS   PREFIX.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FJH01001  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  NIT-ST                   PIC  X(02).
     03  DSP-ST                   PIC  X(02).
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
     03  EOF-FLG                  PIC  X(01)  VALUE  SPACE.
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                      PIC  X(01)  VALUE  SPACE.
*
     03  X                        PIC  9(02)  VALUE  ZERO.
     03  IX                       PIC  9(02)  VALUE  ZERO.
     03  REC-CNT                  PIC  9(03)  VALUE  ZERO.
     03  PAGE-CNT                 PIC  9(03)  VALUE  ZERO.
     03  WK-JKTIME                PIC  9(04).
     03  WK-JKTIME-R  REDEFINES   WK-JKTIME.
         05  WK-JKTIME-HH         PIC  9(02).
         05  WK-JKTIME-MM         PIC  9(02).
*抽出条件　
 01  WRK-SELECT-AREA.
     03  WK-DATE1                 PIC  9(08).
     COPY     JHMNITF   OF        XFDLIB
              JOINING   ZENPG     PREFIX.
     COPY     JHMNITF   OF        XFDLIB
              JOINING   JIPG      PREFIX.
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
     03  PF-MSG2.
         05  FILLER               PIC   N(20)
      VALUE NC"_取消_終了_前頁_次頁".
 01  PF-MSG-AREA-R       REDEFINES     PF-MSG-AREA.
     03  PF-MSG-R   OCCURS   2   PIC   N(20).

*メッセージの取得
 01  ERR-MSG-AREA.
     03  ERR-MSG1.
         05  FILLER              PIC   N(20)
             VALUE NC"送信日付を正しく入力して下さい".
     03  ERR-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"キーが無効です".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"送信日付　論理エラー".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"前頁はありません".
     03  ERR-MSG5.
         05  FILLER              PIC   N(20)
             VALUE NC"次頁はありません".
     03  ERR-MSG6.
         05  FILLER              PIC   N(20)
             VALUE NC"対象データが存在しません".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  6   PIC   N(20).
*
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  FAX-ERR           PIC N(15) VALUE
                        NC"日時更新条件Ｍエラー".
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
 NIT-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JHMNITF.
     MOVE        NIT-ST    TO        E-ST.
     MOVE        "JHMNITF" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     FAX-ERR   UPON      CONS.
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
     OPEN      I-O              JHMNITF.
     OPEN      I-O              DSPFILE.
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
*画面初期表示　　　
         WHEN      "1"       PERFORM   DSP-INIT-SEC
*明細入力　　　　　
         WHEN      "2"       PERFORM   DSP-MEIS-SEC
*確認入力   　　　　
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
     CLOSE             DSPFILE   JHMNITF.
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
     MOVE    SPACE                TO   DSP-FJH01001.
*システム日付転送
     MOVE    HEN-DATE             TO   DSP-SDATE.
*システム時間転送
     MOVE    HEN-TIME             TO   DSP-STIME.
*項目属性クリア
     PERFORM  DSP-SYOKI-SEC.
*ページ数クリア
     MOVE     1                   TO   PAGE-CNT.
     MOVE     SPACE               TO   EOF-FLG.
*明細項目出力　　
     MOVE     SYS-DATE       TO   NIT-F01.
     START    JHMNITF   KEY  IS   >=   NIT-F01
     END-START.
*日時更新条件ファイル読込　　　
     PERFORM  NIT-NEXT-SUB.
     PERFORM  BODY-WRITE-SEC.
*バッチ_入力へ
     MOVE    "2"                  TO   PSW.
*
 DSP-INIT-EXIT.
     EXIT.
****************************************************************
*             明細出力処理　　　　                             *
****************************************************************
 BODY-WRITE-SEC          SECTION.
     MOVE     "BODY-WRITE-SEC"      TO   S-NAME.
     PERFORM  TABLE-CLEAR-SUB.
*
*    日時更新条件マスタから明細項目セット　　　　
     PERFORM  VARYING IX FROM 1 BY 1 UNTIL IX > 16
                                     OR  EOF-FLG = "1"
                                     OR  EOF-FLG = "E"
***     日付　　
        MOVE     NIT-F01          TO   DSP-DATE(IX)
***     実行区分　
        MOVE     NIT-F02          TO   DSP-JKKBN (IX)
***     発注日　　　
        MOVE     NIT-F05          TO   DSP-HACDAY(IX)
***     納品日　　　
        MOVE     NIT-F06          TO   DSP-NOUDAY(IX)
***     出荷日　　　
        MOVE     NIT-F07          TO   DSP-SYUDAY(IX)
***     実行結果　　　
        MOVE     NIT-F08          TO   DSP-KEKKA1(IX)
        EVALUATE   NIT-F08
             WHEN   SPACE
             MOVE   NC"送信中　"  TO   DSP-KEKKA2(IX)
             WHEN   "01"
             MOVE   NC"送信ＯＫ"  TO   DSP-KEKKA2(IX)
             WHEN   "02"
             MOVE   NC"送信ＮＧ"  TO   DSP-KEKKA2(IX)
             WHEN   "03"
             MOVE   NC"相手話中"  TO   DSP-KEKKA2(IX)
        END-EVALUATE
***     レコード退避
        IF       IX        =      1
                 MOVE      NIT-REC     TO  ZENPG-REC
        END-IF
        IF       IX        =      16
                 MOVE      NIT-REC     TO  JIPG-REC
        END-IF
***
        PERFORM  NIT-NEXT-SUB
     END-PERFORM.
*
 BODY-WRITE-EXIT.
     EXIT.
****************************************************************
*           　明細  入力( PSW = 2 )                2.2       *
****************************************************************
 DSP-MEIS-SEC          SECTION.
     MOVE     "DSP-MEIS-SEC"      TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   MEIS-CHK-SEC
                IF        ERR-FLG  =  ZERO
                          MOVE  "3"   TO   PSW
                END-IF
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*前頁表示
         WHEN   "F011"
                PERFORM MEIS-CHK-SEC
                PERFORM NIT-WRITE-SUB
                PERFORM BEFORE-PAGE-SUB
*次頁表示
         WHEN   "F012"
                PERFORM MEIS-CHK-SEC
                PERFORM NIT-WRITE-SUB
                PERFORM NEXT-PAGE-SUB
         WHEN   OTHER
                MOVE     2       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-HEAD-EXIT.
     EXIT.
****************************************************************
*             　明細チェック                         2.2.1     *
****************************************************************
 MEIS-CHK-SEC             SECTION.
     MOVE     "MEIS-CHK-SEC"     TO   S-NAME.
*
***  実行区分未入力チェック　　　　
     PERFORM  VARYING X FROM 1 BY 1 UNTIL X > 16
                                       OR DSP-DATE(X) = SPACE
         IF       DSP-JKKBN (X)  =    SPACE
              MOVE   1       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-JKKBN(X)
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-JKKBN(X)
         END-IF
     END-PERFORM.
***  実行時間未入力・論理チェック
     PERFORM  VARYING X FROM 1 BY 1 UNTIL X > 16
                                       OR DSP-DATE(X) = SPACE
         IF   DSP-JKTIME(X)  =      SPACE
              MOVE   1       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-JKTIME(X)
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-JKTIME(X)
         ELSE
*             データ退避
              MOVE   DSP-JKTIME(X)     TO      WK-JKTIME
              IF     WK-JKTIME-HH   <  ZERO
              OR     WK-JKTIME-HH   >  23
                     MOVE  1  TO ERR-FLG
                     MOVE "R" TO EDIT-OPTION OF DSP-JKTIME(X)
                     MOVE "C" TO EDIT-CURSOR OF DSP-JKTIME(X)
              END-IF
              IF     WK-JKTIME-MM   NOT = ZERO AND 15
                                               AND 30 AND 45
                     MOVE  1  TO ERR-FLG
                     MOVE "R" TO EDIT-OPTION OF DSP-JKTIME(X)
                     MOVE "C" TO EDIT-CURSOR OF DSP-JKTIME(X)
              END-IF
         END-IF
     END-PERFORM.
***  発注日論理エラーチェック　　　
     PERFORM  VARYING X FROM 1 BY 1 UNTIL X > 16
                                       OR DSP-DATE(X) = SPACE
         MOVE     "2"            TO   LINK-IN-KBN
         MOVE     ZERO           TO   LINK-IN-YMD6
         MOVE     DSP-HACDAY(X)  TO   LINK-IN-YMD8
         MOVE     ZERO           TO   LINK-OUT-RET
         MOVE     ZERO           TO   LINK-OUT-YMD
         CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                         LINK-IN-YMD6
                                         LINK-IN-YMD8
                                         LINK-OUT-RET
                                         LINK-OUT-YMD
         IF   LINK-OUT-RET   NOT = ZERO
              MOVE  3        TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-HACDAY(X)
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-HACDAY(X)
         END-IF
     END-PERFORM.
***  納品日未入力・論理エラーチェック
     PERFORM  VARYING X FROM 1 BY 1 UNTIL X > 16
                                       OR DSP-DATE(X) = SPACE
         IF       DSP-NOUDAY(X) NOT NUMERIC OR
                  DSP-NOUDAY(X) =  SPACE
                  MOVE   1    TO   ERR-FLG
                  MOVE  "R"   TO   EDIT-OPTION  OF  DSP-NOUDAY(X)
                  MOVE  "C"   TO   EDIT-CURSOR  OF  DSP-NOUDAY(X)
         ELSE
                  MOVE     ZERO           TO   LINK-IN-YMD6
                  MOVE     DSP-HACDAY(X)  TO   LINK-IN-YMD8
                  MOVE     ZERO           TO   LINK-OUT-RET
                  MOVE     ZERO           TO   LINK-OUT-YMD
                  CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                                  LINK-IN-YMD6
                                                  LINK-IN-YMD8
                                                  LINK-OUT-RET
                                                  LINK-OUT-YMD
                  IF   LINK-OUT-RET   NOT = ZERO
                       MOVE  3        TO   ERR-FLG
                       MOVE "R" TO EDIT-OPTION OF  DSP-HACDAY(X)
                       MOVE "C" TO EDIT-CURSOR OF  DSP-HACDAY(X)
                  END-IF
         END-IF
     END-PERFORM.
***  出荷日論理エラーチェック　　　
     PERFORM  VARYING X FROM 1 BY 1 UNTIL X > 16
                                       OR DSP-DATE(X) = SPACE
         MOVE     "2"            TO   LINK-IN-KBN
         MOVE     ZERO           TO   LINK-IN-YMD6
         MOVE     DSP-SYUDAY(X)  TO   LINK-IN-YMD8
         MOVE     ZERO           TO   LINK-OUT-RET
         MOVE     ZERO           TO   LINK-OUT-YMD
         CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                         LINK-IN-YMD6
                                         LINK-IN-YMD8
                                         LINK-OUT-RET
                                         LINK-OUT-YMD
         IF   LINK-OUT-RET   NOT = ZERO
              MOVE  3        TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-SYUDAY(X)
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-SYUDAY(X)
         END-IF
     END-PERFORM.
*
 MEIS-CHK-EXIT.
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
                PERFORM       NIT-WRITE-SUB
                MOVE    "1"      TO   PSW
***             項目初期化
                MOVE    SPACE    TO   RD-FLG
                MOVE    ZERO     TO   IX
***
                CLOSE   JHMNITF
                OPEN    INPUT    JHMNITF
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
***             項目初期化
                MOVE    SPACE    TO   RD-FLG
                MOVE    ZERO     TO   IX
***
                CLOSE   JHMNITF
                OPEN    INPUT    JHMNITF
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*前頁表示
         WHEN   "F011"
                PERFORM MEIS-CHK-SEC
                PERFORM NIT-WRITE-SUB
                PERFORM BEFORE-PAGE-SUB
*次頁表示
         WHEN   "F012"
                PERFORM MEIS-CHK-SEC
                PERFORM NIT-WRITE-SUB
                PERFORM NEXT-PAGE-SUB
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
*    IF    PSW     =    2
*          MOVE    PF-MSG1             TO   DSP-PFMSG
*    END-IF.
*    IF    PSW     =    3
*          MOVE    PF-MSG2             TO   DSP-PFMSG
*    END-IF.
     MOVE    PF-MSG2                   TO   DSP-PFMSG.
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FJH01001"          TO   DSP-FMT.
     WRITE    DSP-FJH01001.
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
                MOVE    "BODY"    TO   DSP-GRP
*確認
         WHEN   "3"
                MOVE    "KAKNIN"  TO   DSP-GRP
     END-EVALUATE.

     MOVE    "FJH01001"           TO   DSP-FMT.
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
     PERFORM  VARYING X FROM 1 BY 1 UNTIL X > 16
                                       OR DSP-DATE(X) = SPACE
              MOVE "M"   TO EDIT-OPTION OF DSP-JKKBN (X)
              MOVE SPACE TO EDIT-CURSOR OF DSP-JKKBN (X)
              MOVE "M"   TO EDIT-OPTION OF DSP-JKTIME(X)
              MOVE SPACE TO EDIT-CURSOR OF DSP-JKTIME(X)
              MOVE "M"   TO EDIT-OPTION OF DSP-HACDAY(X)
              MOVE SPACE TO EDIT-CURSOR OF DSP-HACDAY(X)
              MOVE "M"   TO EDIT-OPTION OF DSP-NOUDAY(X)
              MOVE SPACE TO EDIT-CURSOR OF DSP-NOUDAY(X)
              MOVE "M"   TO EDIT-OPTION OF DSP-SYUDAY(X)
              MOVE SPACE TO EDIT-CURSOR OF DSP-SYUDAY(X)
     END-PERFORM.
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*             前頁表示処理　　　　　　　　                     *
****************************************************************
 BEFORE-PAGE-SUB       SECTION.
     MOVE     "BEFORE-PAGE-SUB"  TO   S-NAME.
*データ検索
     MOVE      ZENPG-REC    TO        NIT-REC.
     PERFORM   NIT-BACKSTART-SUB.
     IF        EOF-FLG          =     ZERO
               PERFORM          NIT-NEXT-SUB
     END-IF.
     IF        EOF-FLG      NOT =     ZERO
               MOVE         4   TO    ERR-FLG
               GO               TO    BEFORE-PAGE-EXIT
     END-IF.
*
     MOVE      SPACE            TO    JIPG-REC.
     SUBTRACT    1              FROM  PAGE-CNT.
*    画面明細表示
     PERFORM  TABLE-CLEAR-SUB.
*
*    日時更新条件マスタから明細項目セット　　　　
     PERFORM  VARYING IX FROM 16 BY -1 UNTIL IX = 0
                                     OR  EOF-FLG = "1"
                                     OR  EOF-FLG = "E"
***     日付　　
        MOVE     NIT-F01          TO   DSP-DATE(IX)
***     実行区分　
        MOVE     NIT-F02          TO   DSP-JKKBN (IX)
***     発注日　　　
        MOVE     NIT-F05          TO   DSP-HACDAY(IX)
***     納品日　　　
        MOVE     NIT-F06          TO   DSP-NOUDAY(IX)
***     出荷日　　　
        MOVE     NIT-F07          TO   DSP-SYUDAY(IX)
***     実行結果　　　
        MOVE     NIT-F08          TO   DSP-KEKKA1(IX)
        EVALUATE   NIT-F08
             WHEN   SPACE
             MOVE   NC"送信中　"  TO   DSP-KEKKA2(IX)
             WHEN   "01"
             MOVE   NC"送信ＯＫ"  TO   DSP-KEKKA2(IX)
             WHEN   "02"
             MOVE   NC"送信ＮＧ"  TO   DSP-KEKKA2(IX)
             WHEN   "03"
             MOVE   NC"相手話中"  TO   DSP-KEKKA2(IX)
        END-EVALUATE
***     レコード退避
        IF       IX        =      1
                 MOVE      NIT-REC     TO  ZENPG-REC
        END-IF
        IF       IX        =      16
                 MOVE      NIT-REC     TO  JIPG-REC
        END-IF
***
        PERFORM  NIT-NEXT-SUB
     END-PERFORM.
*
 BEFORE-PAGE-EXIT.
     EXIT.
****************************************************************
*             次頁表示処理　　　　　　　　                     *
****************************************************************
 NEXT-PAGE-SUB       SECTION.
     MOVE     "NEXT-PAGE-SUB"  TO   S-NAME.
*次頁存在チェック
     IF   JIPG-REC     =   SPACE
          MOVE      5    TO   ERR-FLG
          GO        TO   NEXT-PAGE-EXIT
     END-IF.
*データ検索
     MOVE      JIPG-REC    TO        NIT-REC.
     PERFORM   NIT-NEXTSTART-SUB.
     IF        EOF-FLG          =     ZERO
               PERFORM           NIT-NEXT-SUB
     END-IF.
     IF        EOF-FLG      NOT =     ZERO
               MOVE         5   TO    ERR-FLG
               GO               TO    NEXT-PAGE-EXIT
     END-IF.
*
     MOVE      SPACE            TO    JIPG-REC.
     ADD         1              TO    PAGE-CNT.
*    画面明細表示
     PERFORM   BODY-WRITE-SEC.
*
 NEXT-PAGE-EXIT.
     EXIT.
****************************************************************
*             日時更新条件マスタ　　ＢＡＣＫ                   *
****************************************************************
 NIT-BACKSTART-SUB     SECTION.
     MOVE     "NIT-BACKSTART-SUB"  TO   S-NAME.
*
     START    JHMNITF    KEY       <    NIT-F01
                                        REVERSED
        INVALID  KEY
              MOVE       "E"       TO   EOF-FLG
        NOT INVALID KEY
              MOVE       ZERO      TO   EOF-FLG
     END-START.
*
 FAX-BACKSTART-EXIT.
     EXIT.
****************************************************************
*             日時更新条件マスタ　　ＮＥＸＴ                   *
****************************************************************
 NIT-NEXTSTART-SUB     SECTION.
     MOVE     "NIT-NEXTSTART-SUB"  TO   S-NAME.
*
     START    JHMNITF    KEY       >    NIT-F01
        INVALID  KEY
              MOVE       "E"       TO   EOF-FLG
        NOT INVALID KEY
              MOVE       ZERO      TO   EOF-FLG
     END-START.
*
 FAX-NEXTSTART-EXIT.
     EXIT.
****************************************************************
*             日時更新条件マスタ　　順読込　                   *
****************************************************************
 NIT-NEXT-SUB     SECTION.
     MOVE     "NIT-NEXT-SUB"  TO   S-NAME.
*
     READ      JHMNITF        NEXT   AT   END
               MOVE     "E"   TO     EOF-FLG
     END-READ.
 FAX-NEXT-EXIT.
     EXIT.
****************************************************************
*             テーブルクリア　　　　　　　　                   *
****************************************************************
 TABLE-CLEAR-SUB  SECTION.
     MOVE     "TABLE-CLEAR-SUB"  TO   S-NAME.
*
     PERFORM  VARYING X FROM 1 BY 1 UNTIL X > 16
              MOVE   SPACE    TO     DSP-MAS001(X)
     END-PERFORM.
 TABLE-CLEAR-EXIT.
     EXIT.
****************************************************************
*             日時更新条件マスタ更新　　　　                   *
****************************************************************
 NIT-WRITE-SUB    SECTION.
     MOVE     "NIT-WRITE-SUB"  TO   S-NAME.
*
     PERFORM  VARYING X FROM 1 BY 1 UNTIL X > 16
                                       OR DSP-DATE(X) = SPACE
              MOVE   DSP-DATE(X)    TO   NIT-F01
              MOVE   DSP-JKKBN(X)   TO   NIT-F02
              MOVE   DSP-JKTIME(X)  TO   NIT-F03
              MOVE   DSP-HACDAY(X)  TO   NIT-F05
              MOVE   DSP-NOUDAY(X)  TO   NIT-F06
              MOVE   DSP-SYUDAY(X)  TO   NIT-F07
              REWRITE    NIT-REC
     END-PERFORM.
 NIT-WRITE-EXIT.
     EXIT.
*******************< PROGRAM-END SJH0015I >*********************
