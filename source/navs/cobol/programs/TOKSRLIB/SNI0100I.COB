****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　Ｄ３６５連携　　　　　　　　　　　*
*    モジュール名　　　　：　日次計上ＤＴ件数照会　　　　　　　*
*    作成日／作成者　　　：　2024/03/21   INOUE                *
*    処理内容　　　　　　：　照会条件を入力して　　　　　　　　*
*    　　　　　　　　　　　　日次計上件数の照会を行なう。　　　*
*    変更日／変更者　　　：　                                  *
*    変更内容　　　　　　：　                                  *
*    　　　　　　　　　　　　                                  *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SNI0100I.
*                流用：NVS0070I.
 AUTHOR.               ASS.
 DATE-WRITTEN.         2024/03/21.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*---<<  画面定義ファイル  >>--*
     SELECT   DSPFILE        ASSIGN        TO  GS-DSPF
                             FORMAT        IS  DSP-FMT
                             GROUP         IS  DSP-GRP
                             PROCESSING    IS  DSP-PRO
                             FUNCTION      IS  DSP-FNC
                             FILE STATUS   IS  DSP-ST.
*---<<  日次計上件数ファイル  >>--*
     SELECT   DNITKNL2       ASSIGN        TO  01-VI-DNITKNL2
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  SEQUENTIAL
                             RECORD KEY    IS  DNI-F01
                                               DNI-F02
                             FILE STATUS   IS  DNI-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FNI01001  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
*    FILE = 日次計上件数ファイル                               *
****************************************************************
 FD  DNITKNL2
                        LABEL     RECORD    IS   STANDARD.
                        COPY      DNITKNL2  OF   XFDLIB
                        JOINING   DNI       AS   PREFIX.
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  DSP-ST                   PIC  X(02).
     03  DNI-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
***  プログラムスイッチ（画面遷移制御）
 01  PSW                          PIC  X(01)  VALUE  SPACE.
*フラグ領域
 01  FLG-AREA.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  INV-FLG                  PIC  9(01)  VALUE  ZERO.
     03  READ-FLG                 PIC  9(01)  VALUE  ZERO.
     03  ERR-FLG                  PIC  9(02)  VALUE  ZERO.
     03  ZSHIMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  HTANMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  HMEIMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  HJYOKEN-INV-FLG          PIC  X(03)  VALUE  SPACE.
     03  HTOKMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  BUMONF-INV-FLG           PIC  X(03)  VALUE  SPACE.
*カウント領域
 01  CNT-AREA.
     03  P-CNT                    PIC  9(07)  VALUE  ZERO.
     03  C-CNT                    PIC  9(07)  VALUE  ZERO.
     03  S-CNT                    PIC  9(07)  VALUE  ZERO.
     03  MAX-CNT                  PIC  9(07)  VALUE  ZERO.
     03  OUT-CNT                  PIC  9(07)  VALUE  ZERO.
     03  IX1                      PIC  9(03)  VALUE  ZERO.
*ワーク領域
 01  WRK-AREA.
     03  WK-YOTEIBI               PIC  9(08).
     03  WK-BDY-KEY.
         05  WK-BDY-F02           PIC  9(07).
*    03  WK-SHOCD.
*        05  WK-SHO               PIC  X(01)  OCCURS 8.
 01  WK-TANA.
     03  WK-TANA1                 PIC  X(01).
     03  FILLER                   PIC  X(01)  VALUE  "-".
     03  WK-TANA2                 PIC  X(03).
     03  FILLER                   PIC  X(01)  VALUE  "-".
     03  WK-TANA3                 PIC  X(02).
 01  WK-AMARI                     PIC  9(02)  VALUE 0.
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
 01  EDT-DATE.
     03  EDT-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  EDT-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  EDT-DATE-DD              PIC  9(02)  VALUE  ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
 01  EDT-TIME.
     03  EDT-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  EDT-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  EDT-TIME-SS              PIC  9(02)  VALUE  ZERO.
*特販部名称編集
 01  HEN-TOKHAN-AREA.
     03  FILLER                   PIC  N(01)  VALUE  NC"（".
     03  HEN-TOKHAN               PIC  N(06)  VALUE  SPACE.
     03  FILLER                   PIC  N(01)  VALUE  NC"）".
*
 01  TABLE-AREA.
     03  TBL-YOUBI                PIC   9(01).

     03  TABLE-G.
       05  TABLE1      OCCURS  7.
           07  TABLE2  OCCURS 16.
               09  TBL-MSJDT        PIC   9(08).
               09  TBL-MSJTM        PIC   9(06).
               09  TBL-MSJYU        PIC   9(06).
               09  TBL-MSURI        PIC   9(06).
               09  TBL-MJNB         PIC   9(06).
               09  TBL-MSUNB        PIC   9(06).
               09  TBL-MSTYO        PIC   9(06).
               09  TBL-MSSGK        PIC   9(07).
               09  TBL-MSSA         PIC   9(05).
     03  TABLE-G-R  REDEFINES TABLE-G.
         05  TABLE2-R  OCCURS 112.
             09  TBL-MSJDT-R        PIC   9(08).
             09  TBL-MSJTM-R        PIC   9(06).
             09  TBL-MSJYU-R        PIC   9(06).
             09  TBL-MSURI-R        PIC   9(06).
             09  TBL-MJNB-R         PIC   9(06).
             09  TBL-MSUNB-R        PIC   9(06).
             09  TBL-MSTYO-R        PIC   9(06).
             09  TBL-MSSGK-R        PIC   9(07).
             09  TBL-MSSA-R         PIC   9(05).
*ＰＦガイド
 01  PF-MSG-AREA.
     03  PF-MSG1                  PIC  N(20)  VALUE
         NC"_取消_終了".
     03  PF-MSG2                  PIC  N(20)  VALUE
         NC"_取消_終了_項目戻り_前頁_次頁".
 01  PF-MSG-AREA-R       REDEFINES     PF-MSG-AREA.
     03  PF-MSG-R   OCCURS   2    PIC   N(20).
*
*メッセージの取得
 01  ERR-MSG-AREA.
     03  ERR-MSG1                 PIC  N(20)  VALUE
         NC"該当データは存在しません。".
     03  ERR-MSG2                 PIC  N(20)  VALUE
         NC"無効キーです。".
     03  ERR-MSG3                 PIC  N(20)  VALUE
         NC"前頁はありません。　　　".
     03  ERR-MSG4                 PIC  N(20)  VALUE
         NC"次頁はありません。　　　".
     03  ERR-MSG5                 PIC  N(20)  VALUE
         NC"　　　　　　　　　　".
     03  ERR-MSG6                 PIC  N(20)  VALUE
         NC"照会基準日が違います。".
     03  ERR-MSG7                 PIC  N(20)  VALUE
         NC"　　　　　　　　　　".
     03  ERR-MSG8                 PIC  N(20)  VALUE
         NC"　　　　　　　　　".
     03  ERR-MSG9                 PIC  N(20)  VALUE
         NC"　　　　　　　　　　　　　".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R       OCCURS   9    PIC  N(20).
*
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  DSP-ERR  PIC N(15) VALUE
         NC"画面ファイルエラー".
     03  DNI-ERR  PIC N(15) VALUE
         NC"日次計上件数ファイルエラー".
     03  SN3-ERR  PIC N(15) VALUE
         NC"　　　　　　　　　　　　　　".
     03  TAN-ERR  PIC N(15) VALUE
         NC"　　　　　　　　　".
     03  JYO-ERR  PIC N(15) VALUE
         NC"条件ファイルエラー".
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
*LINKAGE               SECTION.
**************************************************************
*01  LINK-BUMNCD           PIC X(04).
*01  LINK-TANTCD           PIC X(02).
**************************************************************
 PROCEDURE             DIVISION.
*PROCEDURE             DIVISION     USING LINK-BUMNCD
*                                         LINK-TANTCD.
**************************************************************
 DECLARATIVES.
 DSP-SEC                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPFILE.
     MOVE        DSP-ST    TO        E-ST.
     MOVE        "DSPFILE" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     DSP-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 DNI-SEC                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DNITKNL2.
     MOVE        DNI-ST    TO        E-ST.
     MOVE       "DNITKNL2" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     DNI-ERR   UPON      CONS.
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
     OPEN      INPUT    DNITKNL2.
     OPEN      I-O      DSPFILE.
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
**
     EVALUATE      PSW
*初期画面表示
         WHEN      "1"       PERFORM   DSP-INIT-SEC
*ヘッダ部入力
         WHEN      "2"       PERFORM   DSP-HEAD-SEC
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
     CLOSE             DSPFILE.
     CLOSE             DNITKNL2.
**
 END-EXIT.
     EXIT.
****************************************************************
*             初期画面表示( PSW = 1 )                2.1       *
****************************************************************
 DSP-INIT-SEC          SECTION.
     MOVE     "DSP-INIT-SEC"      TO   S-NAME.
**
     MOVE     SPACE               TO   DSP-PRO.
*
     MOVE    SPACE                TO   DSP-FNI01001.
     MOVE    "SNI0100I"           TO   DSP-PGID.
     MOVE    "FNI01001"           TO   DSP-FORMID.
     MOVE    HEN-DATE             TO   DSP-SDATE.
     MOVE    HEN-TIME             TO   DSP-STIME.
*項目属性クリア
     PERFORM  DSP-SYOKI-SEC.
*ヘッダ部入力へ
     MOVE     "2"                TO   PSW.
*
 DSP-INIT-EXIT.
     EXIT.
****************************************************************
*             ヘッダ部入力( PSW = 2 )                2.2       *
****************************************************************
 DSP-HEAD-SEC          SECTION.
     MOVE     "DSP-HEAD-SEC"      TO   S-NAME.
**
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"       PERFORM   HEAD-CHK-SEC
*取消
         WHEN   "F004"       MOVE    "1"      TO   PSW
*終了
         WHEN   "F005"       MOVE    "END"    TO   END-FLG
*他
         WHEN    OTHER       MOVE     2       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-HEAD-EXIT.
     EXIT.
****************************************************************
*             ヘッダ部チェック                 2.2.1           *
****************************************************************
 HEAD-CHK-SEC             SECTION.
     MOVE     "HEAD-CHK-SEC"     TO   S-NAME.
*
***  日付範囲入力チェック
     IF     ( DSP-HDDT  NOT =  ZERO ) AND
            ( DSP-HDDT  IS  NUMERIC )
***           論理チェック
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     DSP-HDDT       TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   MOVE  6        TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-HDDT
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-HDDT
                   GO             TO   HEAD-CHK-EXIT
              END-IF
*
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-HDDT
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-HDDT
     END-IF.
*該当データ検索
     IF       ERR-FLG  =  ZERO
              MOVE    1        TO  P-CNT
              PERFORM      DNITKNL2-START-SEC
*
              DIVIDE   OUT-CNT  BY  16
                       GIVING       MAX-CNT
                       REMAINDER    WK-AMARI
              IF       WK-AMARI  NOT = 0
                       ADD  1    TO MAX-CNT
              END-IF
*
*T
*             DISPLAY  "OUT-CNT="  OUT-CNT UPON CONS
*             DISPLAY  "MAX-CNT="  MAX-CNT UPON CONS
*T
     END-IF.
*ワークへデータセット
     IF       ERR-FLG  =  ZERO
              PERFORM     MST-WORK-SEC
     END-IF.
*
     IF       ERR-FLG  =  1
              MOVE    "R"      TO   EDIT-OPTION OF DSP-HDDT
     END-IF.
*
     IF       ERR-FLG  =  ZERO
              MOVE  "3"             TO   PSW
     END-IF.
 HEAD-CHK-EXIT.
     EXIT.
****************************************************************
*             該当データの検索                                 *
****************************************************************
 MST-WORK-SEC          SECTION.
     MOVE     "MST-WORK-SEC"   TO   S-NAME.
**
*対象データ抽出
     MOVE      1               TO   P-CNT.
     MOVE      0               TO   READ-FLG.
     PERFORM   WORK-DSP-SEC.
 MST-WORK-EXIT.
     EXIT.
****************************************************************
*             ワーク→画面表示                                 *
****************************************************************
 WORK-DSP-SEC        SECTION.
     MOVE     "WORK-DSP-SEC"   TO   S-NAME.
*
     MOVE     SPACE              TO   DSP-MAS001(1)
                                      DSP-MAS001(2)
                                      DSP-MAS001(3)
                                      DSP-MAS001(4)
                                      DSP-MAS001(5)
                                      DSP-MAS001(6)
                                      DSP-MAS001(7)
                                      DSP-MAS001(8)
                                      DSP-MAS001(9)
                                      DSP-MAS001(10)
                                      DSP-MAS001(11)
                                      DSP-MAS001(12)
                                      DSP-MAS001(13)
                                      DSP-MAS001(14)
                                      DSP-MAS001(15)
                                      DSP-MAS001(16).
*
     PERFORM  VARYING  IX1  FROM 1  BY  1
                 UNTIL IX1  >   16
         IF TBL-MSJDT    (P-CNT IX1)   NOT =  ZERO
            MOVE TBL-MSJDT   (P-CNT IX1)   TO  DSP-MSJDT    (IX1)
            MOVE TBL-MSJTM   (P-CNT IX1)   TO  DSP-MSJTM    (IX1)
            MOVE TBL-MSJYU   (P-CNT IX1)   TO  DSP-MSJYU    (IX1)
            MOVE TBL-MSURI   (P-CNT IX1)   TO  DSP-MSURI    (IX1)
            MOVE TBL-MJNB    (P-CNT IX1)   TO  DSP-MJNB     (IX1)
            MOVE TBL-MSUNB   (P-CNT IX1)   TO  DSP-MSUNB    (IX1)
            MOVE TBL-MSTYO   (P-CNT IX1)   TO  DSP-MSTYO    (IX1)
            MOVE TBL-MSSGK   (P-CNT IX1)   TO  DSP-MSSGK    (IX1)
            MOVE TBL-MSSA    (P-CNT IX1)   TO  DSP-MSSA     (IX1)
         END-IF
     END-PERFORM.
*
 WORK-DSP-EXIT.
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
     EVALUATE  DSP-FNC
*      実行
       WHEN  "E000"
         MOVE  "3"               TO  PSW
*      取消
       WHEN  "F004"
         MOVE  1                 TO  P-CNT
         MOVE  "1"               TO  PSW
*         PERFORM  INIT-DSP-SEC
*      終了
       WHEN  "F005"
         MOVE  "END"             TO  END-FLG
*      項目戻し
       WHEN  "F006"
         MOVE  "2"               TO  PSW
*      前頁
       WHEN  "F011"
         COMPUTE  C-CNT = P-CNT - 1
         IF  C-CNT = ZERO
             MOVE  3             TO  ERR-FLG
         ELSE
             COMPUTE  P-CNT = P-CNT - 1
             PERFORM  WORK-DSP-SEC
             MOVE "3"            TO  PSW
         END-IF
*      次頁
       WHEN  "F012"
         MOVE  P-CNT             TO  S-CNT
         COMPUTE  C-CNT = P-CNT + 1
         IF ( C-CNT  >   MAX-CNT ) OR
            ( C-CNT  >   7       )
             MOVE  4             TO  ERR-FLG
         ELSE
                 COMPUTE  P-CNT = P-CNT + 1
                 PERFORM  WORK-DSP-SEC
                 MOVE  "3"       TO  PSW

         END-IF

       WHEN   OTHER
         MOVE  2                 TO  ERR-FLG
     END-EVALUATE.
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
     EVALUATE   PSW
         WHEN   "1"
         WHEN   "2"
                MOVE    PF-MSG-R(1)        TO   DSP-PFMSG
         WHEN   "3"
                MOVE    PF-MSG-R(2)        TO   DSP-PFMSG
     END-EVALUATE.
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FNI01001"          TO   DSP-FMT.
     WRITE    DSP-FNI01001.
*
 DSP-WRITE-EXIT.
     EXIT.
****************************************************************
*             画面読込処理                                     *
****************************************************************
 DSP-READ-SEC          SECTION.
     MOVE     "DSP-READ-SEC"      TO   S-NAME.
**
     MOVE    "NE"                 TO   DSP-PRO.
     EVALUATE   PSW
         WHEN   "2"
                MOVE    "HED001"  TO   DSP-GRP
         WHEN   "3"
                MOVE    "KAKU"    TO   DSP-GRP
     END-EVALUATE.
     MOVE    "FNI01001"           TO   DSP-FMT.
     READ    DSPFILE.
*
 DSP-READ-010.
     PERFORM  DSP-SYOKI-SEC.
     MOVE    ZERO                 TO   ERR-FLG.
     MOVE    SPACE                TO   DSP-PRO.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*             画面制御項目初期化                               *
****************************************************************
 DSP-SYOKI-SEC         SECTION.
     MOVE     "DSP-SYOKI-SEC"    TO   S-NAME.
*
     MOVE   "M"        TO  EDIT-OPTION OF DSP-HDDT.
     MOVE   SPACE      TO  EDIT-CURSOR OF DSP-HDDT.
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*    日次計上件数ファイル スタート
****************************************************************
 DNITKNL2-START-SEC     SECTION.
     MOVE     "DNITKNL2-START-SEC" TO   S-NAME.
**
*日次計上件数ファイルスタート
     MOVE      SPACE              TO   DNI-REC.
     INITIALIZE                        DNI-REC.
*T
*    DISPLAY "DSP-HDDT=" DSP-HDDT UPON CONS
*T
     IF      ( DSP-HDDT NOT NUMERIC  ) OR
             ( DSP-HDDT =   ZERO     ) OR
             ( DSP-HDDT =   SPACE    )
               MOVE     99999999  TO   DNI-F01
     ELSE
               MOVE     DSP-HDDT  TO   DNI-F01
     END-IF.
     MOVE      999999             TO   DNI-F02.
     START     DNITKNL2  KEY  IS  >=   DNI-F01 DNI-F02
               INVALID
               MOVE  1            TO   ERR-FLG
               GO                 TO   DNITKNL2-START-EXIT
     END-START.
*
     MOVE     ZERO                TO   INV-FLG.
     INITIALIZE                        TABLE-G.
     PERFORM  DNITKNL2-READ-SEC.
     IF     INV-FLG            =  1
            MOVE  1               TO   ERR-FLG
            GO                    TO   DNITKNL2-START-EXIT
     END-IF.
     PERFORM  VARYING  IX1  FROM  1  BY  1
              UNTIL    IX1  >     112
                 OR    INV-FLG  = 1
*
*T↓
*           DISPLAY "DNI-F01=" DNI-F01 UPON CONS
*T↑
            MOVE  DNI-F01              TO TBL-MSJDT-R(IX1)
            MOVE  DNI-F02              TO TBL-MSJTM-R(IX1)
            MOVE  DNI-F03              TO TBL-MSJYU-R(IX1)
            MOVE  DNI-F04              TO TBL-MSURI-R(IX1)
            MOVE  DNI-F06              TO TBL-MJNB-R (IX1)
            MOVE  DNI-F07              TO TBL-MSUNB-R(IX1)
            MOVE  DNI-F05              TO TBL-MSTYO-R(IX1)
************COMPUTE  TBL-MSSGK-R(IX1)  =  TBL-MSJYU-R(IX1)
*                                       + TBL-MSURI-R(IX1)
*                                       + TBL-MJNB-R (IX1)
*                                       + TBL-MSUNB-R(IX1)
************                            + TBL-MSTYO-R(IX1)
************売上＋受注値引＋調整
            COMPUTE  TBL-MSSGK-R(IX1)  =  TBL-MSURI-R(IX1)
                                        + TBL-MJNB-R (IX1)
                                        + TBL-MSTYO-R(IX1)
            COMPUTE  TBL-MSSA-R  (IX1) =  TBL-MSURI-R(IX1)
                                        - TBL-MSJYU-R(IX1)
*
            PERFORM  DNITKNL2-READ-SEC
     END-PERFORM.
*
     COMPUTE   OUT-CNT  = IX1  -  1.
*T
*    DISPLAY "OUT-CNT =" OUT-CNT  UPON CONS.
*T
*
 DNITKNL2-START-EXIT.
     EXIT.
****************************************************************
*    日次計上件数ファイル読込
****************************************************************
 DNITKNL2-READ-SEC      SECTION.
*
     MOVE     "DNITKNL2-READ-SEC"  TO   S-NAME.
**
     READ  DNITKNL2
           AT  END    MOVE   1      TO   INV-FLG
                      GO            TO   DNITKNL2-READ-EXIT
     END-READ.
*
*T↓
*    DISPLAY "DSP-HDDT=" DSP-HDDT UPON CONS.
*    DISPLAY "DNI-F01 =" DNI-F01  UPON CONS.
*    DISPLAY "DNI-F02 =" DNI-F02  UPON CONS.
*T↑
     IF   ( DSP-HDDT      =  ZERO     ) OR
          ( DSP-HDDT    NOT  NUMERIC  ) OR
          ( DSP-HDDT      =  SPACE    )
            CONTINUE
     ELSE
*T
*    DISPLAY "DSP-HDDT=" DSP-HDDT UPON CONS
*    DISPLAY "DNI-F01 =" DNI-F01  UPON CONS
*T
            IF     DSP-HDDT     >=   DNI-F01
                   CONTINUE
            ELSE
                   MOVE   1          TO   INV-FLG
                   GO    TO  DNITKNL2-READ-EXIT
            END-IF
     END-IF.
*
*T
*    DISPLAY "IX1     =" IX1      UPON CONS.
*    DISPLAY "DNI-F01 =" DNI-F01  UPON CONS.
*T
*
 DNITKNL2-READ-EXIT.
     EXIT.
*******************< PROGRAM-END SNI0100I >*********************
