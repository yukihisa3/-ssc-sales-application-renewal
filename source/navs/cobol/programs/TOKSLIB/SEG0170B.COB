****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　営業所データ連携                  *
*    モジュール名　　　　：　営業所配信データリスト            *
*    作成日／作成者　　　：　2000/05/31                        *
*    更新日／更新者　　　：　　　　　　                        *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SEG0170B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          00/05/31.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITU.
 OBJECT-COMPUTER.       FUJITU.
 SPECIAL-NAMES.
         CONSOLE        IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*取引先マスタ
     SELECT     HTOKMS     ASSIGN    TO        TOKMS2
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       TOK-F01
                           FILE      STATUS    TOK-ST.
*プリント定義ファイル
     SELECT     PRTFILE    ASSIGN    TO        GS-PRTF
                PROCESSING MODE      IS        PRT-PROC
                GROUP                IS        PRT-GROUP
                FORMAT               IS        PRT-FORMAT
                CONTROL              IS        PRT-CNTL
                FILE STATUS          IS        PRT-ST.
****************************************************************
 DATA                      DIVISION.
****************************************************************
 FILE                      SECTION.
****************************************************************
*  FILE= 取引先マスタ                                          *
****************************************************************
 FD  HTOKMS
     BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HTOKMS    OF        XFDLIB
     JOINING     TOK       AS        PREFIX.
****************************************************************
*  FILE=プリントファイル                                       *
****************************************************************
 FD  PRTFILE
     LABEL       RECORD    IS        OMITTED.
     COPY        FEG01701  OF        XMDLIB
     JOINING     PRT       AS        PREFIX.
******************************************************************
 WORKING-STORAGE           SECTION.
******************************************************************
*
***  ｽﾃｰﾀｽｴﾘｱ
 01  FILE-STATUS.
     03  TOK-ST              PIC X(02).
     03  PRT-ST              PIC X(02).
*帳票制御用領域
 01  PRT-CONTROL.
     03  PRT-PROC            PIC  X(02).
     03  PRT-GROUP           PIC  X(08).
     03  PRT-FORMAT          PIC  X(08).
     03  PRT-CNTL            PIC  X(06).
*
 01  FILE-ERR.
     03  TOK-ERR             PIC N(15) VALUE
                        NC"取引先マスタエラー".
     03  PRT-ERR             PIC N(15) VALUE
                        NC"プリンターエラー".
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
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN                  PIC X(01).
 01  LINK-IN-YMD6                 PIC 9(06).
 01  LINK-IN-YMD8                 PIC 9(08).
 01  LINK-OUT-RET                 PIC X(01).
 01  LINK-OUT-YMD                 PIC 9(08).
*
******************************************************************
 LINKAGE           SECTION.
******************************************************************
 01  LINK-DATE                    PIC  9(08).
 01  LINK-TIME                    PIC  9(04).
 01  LINK-TORICD                  PIC  X(08).
 01  LINK-KEKKACD                 PIC  9(01).
****************************************************************
*             PROCEDURE           DIVISION                     *
****************************************************************
 PROCEDURE    DIVISION     USING  LINK-DATE  LINK-TIME
                                  LINK-TORICD  LINK-KEKKACD.
 DECLARATIVES.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ERR   UPON      CONS.
     DISPLAY     TOK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 PRT-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE PRTFILE.
     DISPLAY     PRT-ERR   UPON      CONS.
     DISPLAY     PRT-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
****************************************************************
*           　M A I N             M O D U L E         0.0      *
****************************************************************
 MAIN-SEC                  SECTION.
*ファイルＯＰＥＮ
     OPEN        INPUT       HTOKMS.
     OPEN        OUTPUT      PRTFILE.
     MOVE        SPACE       TO   PRT-FEG01701.
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
     MOVE      HEN-DATE           TO   PRT-SDATE.
*
*システム時刻取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
     MOVE      HEN-TIME           TO   PRT-STIME.
*
*項目設定
***  受信日付
     MOVE     LINK-DATE(1:4)      TO        HEN-DATE-YYYY.
     MOVE     LINK-DATE(5:2)      TO        HEN-DATE-MM.
     MOVE     LINK-DATE(7:2)      TO        HEN-DATE-DD.
     MOVE     HEN-DATE            TO        PRT-JDATE.
***  受信時間
     MOVE     LINK-TIME(1:2)      TO        HEN-TIME-HH.
     MOVE     LINK-TIME(3:2)      TO        HEN-TIME-MM.
     MOVE     HEN-TIME(1:5)       TO        PRT-JTIME.
***  受信取引先
     MOVE     LINK-TORICD         TO        PRT-TORICD  TOK-F01.
     READ     HTOKMS
       INVALID
              MOVE      SPACE     TO        PRT-TORINM
       NOT INVALID
              MOVE      TOK-F03   TO        PRT-TORINM
     END-READ.
***  受信結果／エラー内容
     EVALUATE LINK-KEKKACD
     WHEN 1
     MOVE NC"営業所より、上記、バッチ_のオンラインデータを受信"
                             TO        PRT-MSGNM1
     MOVE NC"しました。　　　　　　　　　　　　　　　　　　　　"
                             TO        PRT-MSGNM2
     MOVE NC"営業所受信データの発注集計表の出力を開始して下さい"
                             TO        PRT-MSGNM3
     MOVE NC"※尚、営業所分の発注集計表は、営業所用の発注集計表"
                             TO        PRT-MSGNM4
     MOVE NC"　出力で実行して下さい。　　　　　　　　　　　　　"
                             TO        PRT-MSGNM5
     MOVE NC"　　　　　　　　　　　　　　　　　　　　　　　　　"
                             TO        PRT-MSGNM6
     WHEN 2
     MOVE NC"営業所より、上記、バッチ_のオンラインデータを受信"
                             TO        PRT-MSGNM1
     MOVE NC"しました。データ取込み中に異常が発生しました。　　"
                             TO        PRT-MSGNM2
     MOVE NC"再度、営業所から手動にて、営業所受信分のデータを再"
                             TO        PRT-MSGNM3
     MOVE NC"送信して頂けるように依頼して下さい。　　　　　　　"
                             TO        PRT-MSGNM4
     MOVE NC"　　　　　　　　　　　　　　　　　　　　　　　　　"
                             TO        PRT-MSGNM5
     MOVE NC"　　　　　　　　　　　　　　　　　　　　　　　　　"
                             TO        PRT-MSGNM6
     WHEN OTHER
     MOVE  ALL NC"＊"    TO        PRT-MSGNM1
     MOVE  ALL NC"＊"    TO        PRT-MSGNM2
     MOVE  ALL NC"＊"    TO        PRT-MSGNM3
     MOVE  ALL NC"＊"    TO        PRT-MSGNM4
     MOVE  ALL NC"＊"    TO        PRT-MSGNM5
     MOVE  ALL NC"＊"    TO        PRT-MSGNM6
     END-EVALUATE.
*プリント出力
     MOVE        SPACE       TO   PRT-CONTROL.
     MOVE        "FEG01701"  TO   PRT-FORMAT.
     MOVE        "SCREEN"    TO   PRT-GROUP.
     WRITE       PRT-FEG01701.
*終了処理
     CLOSE       PRTFILE  HTOKMS.
*
 MAIN-SEC-EXIT.
     EXIT.
