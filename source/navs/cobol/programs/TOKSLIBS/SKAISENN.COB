****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＥＤＩ受信サーバー　　　　　　　　*
*    業務名　　　　　　　：　                                  *
*    モジュール名　　　　：　回線使用状況照会                  *
*    作成日／更新日　　　：　2010/12/06                        *
*    作成者／更新者　　　：　ＮＡＶ                            *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SKAISENN.
 AUTHOR.               NAV.
 DATE-WRITTEN.         10/12/06.
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
*ＥＤＩ回線管理マスタ
     SELECT  JSMKAIF   ASSIGN    TO        DA-01-VI-JSMKAIL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       KAI-F01
                                           KAI-F02
                       FILE      STATUS    KAI-ST.
*取引先マスタ
     SELECT  HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TOK-F01
                       FILE      STATUS    TOK-ST.
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
*    FILE = ＥＤＩ回線管理マスタ                               *
****************************************************************
 FD  JSMKAIF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JSMKAIF   OF   XFDLIB
                       JOINING   KAI       AS   PREFIX.
****************************************************************
*    FILE = 取引先マスタ                                       *
****************************************************************
 FD  HTOKMS
                       BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTOKMS    OF   XFDLIB
                       JOINING   TOK       AS   PREFIX.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FKAISENN   OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  KAI-ST                   PIC  X(02).
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
     03  WK-JYU-TOKCD             PIC  9(08)  VALUE  ZERO.
     03  WK-JYU-DTKBN             PIC  X(02)  VALUE  SPACE.
     03  HTOKMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  CHK-FLG                  PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG                  PIC  9(01)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  KAI-END                  PIC  X(03)  VALUE  SPACE.
     03  RD-FLG                   PIC  X(03)  VALUE  SPACE.
     03  SET-FLG                  PIC  X(03)  VALUE  SPACE.
     03  P-CNT                    PIC  9(01)  VALUE  ZERO.
     03  S-CNT                    PIC  9(01)  VALUE  ZERO.
     03  C-CNT                    PIC  9(01)  VALUE  ZERO.
     03  X                        PIC  9(02)  VALUE  ZERO.
     03  Y                        PIC  9(02)  VALUE  ZERO.
**   2010/11/12 ADD ↓
     03  IX                       PIC  9(02)  VALUE  ZERO.
     03  WK-IX                    PIC  9(02)  VALUE  ZERO.
     03  WK-IX2                   PIC  9(02)  VALUE  ZERO.
     03  WK-SEQ                   PIC  9(02)  VALUE  ZERO.
**   2010/11/12 ADD ↑
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                      PIC  X(01)  VALUE  SPACE.
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
*取引先ＣＤ数字変換
 01  WK-TORICD                    PIC  X(08).
 01  WK-TORICD-R                  REDEFINES   WK-TORICD.
     03  WK-TORICD-H              PIC  9(08).
*ＰＦガイド
 01  PF-MSG-AREA.
     03  PF-MSG1.
         05  FILLER               PIC   N(20)
      VALUE NC"_終了　　　　　　　　　　　　　　".
*
*桁数20⇒30  ERR-MSG6 追加 再定義項目定義繰り返し 5 ⇒ 6
*メッセージの取得
 01  ERR-MSG-AREA.
     03  ERR-MSG1.
         05  FILLER              PIC   N(30)
             VALUE NC"無効ＰＦキーです。".
     03  ERR-MSG2.
         05  FILLER              PIC   N(30)
             VALUE NC"前頁はありません。".
     03  ERR-MSG3.
         05  FILLER              PIC   N(30)
             VALUE NC"次頁はありません。".
     03  ERR-MSG4.
         05  FILLER              PIC   N(30)
             VALUE NC"受信取引先を入力して下さい。".
     03  ERR-MSG5.
         05  FILLER              PIC   N(30)
             VALUE NC"選択した番号は受信対象ではありません。".
     03  ERR-MSG6.
         05  FILLER              PIC   N(30)
             VALUE NC"画面に表示されている番号を入力して下さい".
     03  ERR-MSG7.
         05  FILLER              PIC   N(30)
             VALUE NC"この回線は現在、使用出来ません".
     03  ERR-MSG8.
         05  FILLER              PIC   N(30)
     VALUE NC"この回線は現在、空いています。".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  8   PIC   N(30).
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  KAI-ERR           PIC N(15) VALUE
                        NC"ＥＤＩ回線管理マスタエラー".
     03  TOK-ERR           PIC N(15) VALUE
                        NC"取引先マスタエラー".
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
 KAI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JSMKAIF.
     MOVE        KAI-ST    TO        E-ST.
     MOVE        "JSMKAIF" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     KAI-ERR   UPON      CONS.
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
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*ファイルのＯＰＥＮ
     OPEN      I-O              DSPFILE.
     OPEN      INPUT            HTOKMS.
*ワークの初期化
     INITIALIZE                 FLG-AREA.
*初期画面の表示
     MOVE     SPACE               TO   DSP-PRO.
*ヘッド入力へ
     MOVE    "1"                  TO   PSW.
*頁セット
     MOVE     1                   TO   P-CNT.
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
*初期処理(2.1)
         WHEN      "1"       PERFORM   DSP-INIT-SEC
*確認入力    (2.3)
         WHEN      "2"       PERFORM   DSP-KAKU-SEC
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
     CLOSE             DSPFILE   HTOKMS.
**
 END-EXIT.
     EXIT.
****************************************************************
*             初期画面表示                                     *
****************************************************************
 DSP-INIT-SEC          SECTION.
     MOVE     "DSP-INIT-SEC"      TO   S-NAME.
 INITD001.
*初期画面の表示
     MOVE    SPACE                TO   DSP-PRO.
*画面の初期化
     MOVE    SPACE                TO   DSP-FKAISENN.
*フォームＩＤ転送
     MOVE   "FKAISENN"             TO   DSP-FORMID.
*PGID転送
     MOVE   "SKAISENN"            TO   DSP-PGMID.
*システム日付転送
     MOVE    HEN-DATE             TO   DSP-SDATE.
*システム時間転送
     MOVE    HEN-TIME             TO   DSP-STIME.
*回線状態確認
     PERFORM   JSMKAIF-READ-SEC.
*照会日付入力へ
     MOVE     "2"           TO          PSW.
*
 DSP-INIT-EXIT.
     EXIT.
****************************************************************
*             確認処理　入力（ PSW = 3 ）            2.4
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
                MOVE    "2"      TO   PSW
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
         WHEN   OTHER
                MOVE     1       TO   ERR-FLG
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
           MOVE    ZERO               TO   ERR-FLG
     END-IF.
*ガイドメッセージの設定
     MOVE          PF-MSG1            TO   DSP-PFGAID.
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FKAISENN"          TO   DSP-FMT.
     WRITE    DSP-FKAISENN.
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
     MOVE    "KAKU"               TO   DSP-GRP.
*
     MOVE    "FKAISENN"            TO   DSP-FMT.
     READ    DSPFILE.
*入力項目の属性を通常にする
 DSP-READ-010.
*    MOVE    ZERO                 TO   ERR-FLG.
     MOVE    SPACE                TO   DSP-PRO.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*             取引先マスタ読込み                     3.0       *
****************************************************************
 HTOKMS-READ-SEC       SECTION.
*
     MOVE     "HTOKMS-READ-SEC"   TO   S-NAME.
*
     READ HTOKMS  INVALID
          MOVE    "INV"     TO    HTOKMS-INV-FLG
          NOT  INVALID
          MOVE    SPACE     TO    HTOKMS-INV-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
****************************************************************
*             回線環境マスタ読込み                   3.0       *
****************************************************************
 JSMKAIF-READ-SEC      SECTION.
*
     OPEN    INPUT    JSMKAIF.
     MOVE    SPACE        TO     KAI-END.
     MOVE    NC"使用×"   TO     DSP-SEN1 DSP-SEN2 DSP-SEN3
                                 DSP-SEN4 DSP-SEN5 DSP-SEN6.
     MOVE    ERR-MSG7     TO     DSP-SENNM1 DSP-SENNM2 DSP-SENNM3
                                 DSP-SENNM4 DSP-SENNM5 DSP-SENNM6.
*属性初期化
     MOVE    " "          TO     EDIT-OPTION OF DSP-SEN1
                                 EDIT-OPTION OF DSP-SENNM1.
     MOVE    " "          TO     EDIT-OPTION OF DSP-SEN2
                                 EDIT-OPTION OF DSP-SENNM2.
     MOVE    " "          TO     EDIT-OPTION OF DSP-SEN3
                                 EDIT-OPTION OF DSP-SENNM3.
     MOVE    " "          TO     EDIT-OPTION OF DSP-SEN4
                                 EDIT-OPTION OF DSP-SENNM4.
     MOVE    " "          TO     EDIT-OPTION OF DSP-SEN5
                                 EDIT-OPTION OF DSP-SENNM5.
     MOVE    " "          TO     EDIT-OPTION OF DSP-SEN6
                                 EDIT-OPTION OF DSP-SENNM6.
*
     PERFORM VARYING X FROM 1 BY 1 UNTIL X > 6
                                      OR KAI-END = "END"
        READ JSMKAIF
             AT  END
               MOVE   "END"     TO  KAI-END
             NOT  AT  END
               EVALUATE KAI-F01 ALSO KAI-F02  ALSO  KAI-F07
                 WHEN   "1"     ALSO   1      ALSO  " "
                     MOVE NC"　　　"  TO     DSP-SEN1
                     MOVE ERR-MSG8    TO     DSP-SENNM1
                 WHEN   "1"     ALSO   1      ALSO   1
                     MOVE NC"受信中" TO     DSP-SEN1
                     MOVE KAI-F08     TO     WK-TORICD
                     MOVE WK-TORICD-H TO     TOK-F01
                     PERFORM HTOKMS-READ-SEC
                     IF   HTOKMS-INV-FLG = "INV"
                          MOVE ALL NC"＊" TO DSP-SENNM1
                          MOVE "V" TO EDIT-OPTION OF DSP-SEN1
                          MOVE "V" TO EDIT-OPTION OF DSP-SENNM1
                     ELSE
                          MOVE TOK-F02    TO DSP-SENNM1
                          MOVE "V" TO EDIT-OPTION OF DSP-SEN1
                          MOVE "V" TO EDIT-OPTION OF DSP-SENNM1
                     END-IF
                 WHEN   "1"     ALSO   2      ALSO  " "
                     MOVE NC"　　　"  TO     DSP-SEN2
                     MOVE ERR-MSG8    TO     DSP-SENNM2
                 WHEN   "1"     ALSO   2      ALSO   1
                     MOVE NC"受信中"  TO     DSP-SEN2
                     MOVE KAI-F08     TO     WK-TORICD
                     MOVE WK-TORICD-H TO     TOK-F01
                     PERFORM HTOKMS-READ-SEC
                     IF   HTOKMS-INV-FLG = "INV"
                          MOVE ALL NC"＊" TO DSP-SENNM2
                          MOVE "V" TO EDIT-OPTION OF DSP-SEN2
                          MOVE "V" TO EDIT-OPTION OF DSP-SENNM2
                     ELSE
                          MOVE TOK-F02    TO DSP-SENNM2
                          MOVE "V" TO EDIT-OPTION OF DSP-SEN2
                          MOVE "V" TO EDIT-OPTION OF DSP-SENNM2
                     END-IF
                 WHEN   "3"     ALSO   1      ALSO  " "
                     MOVE NC"　　　"  TO     DSP-SEN3
                     MOVE ERR-MSG8    TO     DSP-SENNM3
                 WHEN   "3"     ALSO   1      ALSO   1
                     MOVE NC"受信中"  TO     DSP-SEN3
                     MOVE KAI-F08     TO     WK-TORICD
                     MOVE WK-TORICD-H TO     TOK-F01
                     PERFORM HTOKMS-READ-SEC
                     IF   HTOKMS-INV-FLG = "INV"
                          MOVE ALL NC"＊" TO DSP-SENNM3
                          MOVE "V" TO EDIT-OPTION OF DSP-SEN3
                          MOVE "V" TO EDIT-OPTION OF DSP-SENNM3
                     ELSE
                          MOVE TOK-F02    TO DSP-SENNM3
                          MOVE "V" TO EDIT-OPTION OF DSP-SEN3
                          MOVE "V" TO EDIT-OPTION OF DSP-SENNM3
                     END-IF
                 WHEN   "2"     ALSO   1      ALSO  " "
                     MOVE NC"　　　"  TO     DSP-SEN4
                     MOVE ERR-MSG8    TO     DSP-SENNM4
                 WHEN   "2"     ALSO   1      ALSO   1
                     MOVE NC"受信中"  TO     DSP-SEN4
                     MOVE KAI-F08     TO     WK-TORICD
                     MOVE WK-TORICD-H TO     TOK-F01
                     PERFORM HTOKMS-READ-SEC
                     IF   HTOKMS-INV-FLG = "INV"
                          MOVE ALL NC"＊" TO DSP-SENNM4
                          MOVE "V" TO EDIT-OPTION OF DSP-SEN4
                          MOVE "V" TO EDIT-OPTION OF DSP-SENNM4
                     ELSE
                          MOVE TOK-F02    TO DSP-SENNM4
                          MOVE "V" TO EDIT-OPTION OF DSP-SEN4
                          MOVE "V" TO EDIT-OPTION OF DSP-SENNM4
                     END-IF
                 WHEN   "2"     ALSO   2      ALSO  " "
                     MOVE NC"　　　"  TO     DSP-SEN5
                     MOVE ERR-MSG8    TO     DSP-SENNM5
                 WHEN   "2"     ALSO   2      ALSO   1
                     MOVE NC"受信中"  TO     DSP-SEN5
                     MOVE KAI-F08     TO     WK-TORICD
                     MOVE WK-TORICD-H TO     TOK-F01
                     PERFORM HTOKMS-READ-SEC
                     IF   HTOKMS-INV-FLG = "INV"
                          MOVE ALL NC"＊" TO DSP-SENNM5
                          MOVE "V" TO EDIT-OPTION OF DSP-SEN5
                          MOVE "V" TO EDIT-OPTION OF DSP-SENNM5
                     ELSE
                          MOVE TOK-F02    TO DSP-SENNM5
                          MOVE "V" TO EDIT-OPTION OF DSP-SEN5
                          MOVE "V" TO EDIT-OPTION OF DSP-SENNM5
                     END-IF
                WHEN   "3"     ALSO   2      ALSO  " "
                     MOVE NC"　　　"  TO     DSP-SEN6
                     MOVE ERR-MSG8    TO     DSP-SENNM6
                WHEN   "3"     ALSO   2      ALSO   1
                     MOVE NC"受信中"  TO     DSP-SEN6
                     MOVE KAI-F08     TO     WK-TORICD
                     MOVE WK-TORICD-H TO     TOK-F01
                     PERFORM HTOKMS-READ-SEC
                     IF   HTOKMS-INV-FLG = "INV"
                          MOVE ALL NC"＊" TO DSP-SENNM6
                          MOVE "V" TO EDIT-OPTION OF DSP-SEN6
                          MOVE "V" TO EDIT-OPTION OF DSP-SENNM6
                     ELSE
                          MOVE TOK-F02    TO DSP-SENNM6
                          MOVE "V" TO EDIT-OPTION OF DSP-SEN6
                          MOVE "V" TO EDIT-OPTION OF DSP-SENNM6
                     END-IF
               END-EVALUATE
        END-READ
     END-PERFORM.
     CLOSE    JSMKAIF.
*
 JSMKAIF-READ-EXIT.
     EXIT.
*******************< PROGRAM-END SKAISENN >*********************
