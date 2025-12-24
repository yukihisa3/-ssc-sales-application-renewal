****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ出荷支援システム　　　　　　*
*    業務名　　　　　　　：　作場変更、出荷数変更　　　        *
*    モジュール名　　　　：　ナフコ作場数量一括変更データ抽出　*
*    作成日／作成者　　　：　2018/11/30 NAV TAKAHASHI          *
*    処理概要　　　　　　：　パラメタを受け取り、対象データを　*
*                          　作場数量一括変更データに抽出する。*
*<履歴>*********************************************************
* XXXX/XX/XX XXXXXXXXX ＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮ
* 2018/11/30 高橋_　　新規作成
* 2019/04/23 高橋_　　納品日チェック条件変更　　　　　　　
*　　　　　　　　　　　　　　　　　　　　　　　　　　　　　
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSY3881B.
 AUTHOR.               NAV.
 DATE-WRITTEN.         2018/11/30.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*ナフコ基本情報ファイル
     SELECT  NFJOHOF   ASSIGN    TO        NFJOHOL8
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       JOH-F02
                                           JOH-F03
                                           JOH-F04
                                           JOH-F05
                                           JOH-F09
                                           JOH-F06
                                           JOH-F13
                       FILE      STATUS    JOH-ST.
*作場数量一括変更データ
     SELECT  SKSRHEF   ASSIGN    TO        SKSRHEL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       HEN-F01
                                           HEN-F02
                                           HEN-F03
                                           HEN-F04
                                           HEN-F05
                                           HEN-F09
                                           HEN-F10
                                           HEN-F11
                                           HEN-F12
                                           HEN-F13
                       FILE      STATUS    HEN-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = ナフコ基本情報ファイル
****************************************************************
 FD  NFJOHOF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NFJOHOF   OF   XFDLIB
                       JOINING   JOH       AS   PREFIX.
****************************************************************
*    FILE = 作場数量一括変更データ
****************************************************************
 FD  SKSRHEF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      SKSRHEF   OF   XFDLIB
                       JOINING   HEN       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  JOH-ST                   PIC  X(02).
     03  HEN-ST                   PIC  X(02).
*フラグ領域
 01  FLG-AREA.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  READ-CNT                 PIC  9(07)  VALUE  ZERO.
     03  WT-CNT                   PIC  9(07)  VALUE  ZERO.
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(05)     VALUE " *** ".
     03  S-NAME                   PIC  X(30).
*
*システム日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
     03  WK-TIME6                 PIC  9(06)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-DATE                  PIC  9(06)  VALUE  ZERO.
     03  SYS-DATE                 PIC  9(08).
*
 01  FILE-ERR.
     03  JOH-ERR           PIC N(20) VALUE
                        NC"ナフコ基本情報ファイルエラー".
     03  HEN-ERR           PIC N(20) VALUE
                        NC"作場数量一括変更データエラー".
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
 01  LINK-BUMON          PIC  X(04).
 01  LINK-TANCD          PIC  X(02).
 01  LINK-DATE           PIC  9(08).
 01  LINK-TIME           PIC  9(04).
 01  LINK-TORICD         PIC  X(08).
 01  LINK-SAKUBA         PIC  X(02).
 01  LINK-NOUDT          PIC  9(08).
 01  LINK-NFTENCD        PIC  9(05).
 01  LINK-NFSHOCD        PIC  X(08).
 01  LINK-SAKKBN         PIC  X(01).
 01  LINK-SURKBN         PIC  X(01).
 01  LINK-HEN-SAKUBA     PIC  X(02).
 01  LINK-HEN-SURYO      PIC  9(05).
 01  LINK-UPD-DATE       PIC  9(08).
 01  LINK-UPD-TIME       PIC  9(06).
 01  LINK-KOUSIN         PIC  X(01).
*
**************************************************************
 PROCEDURE       DIVISION  USING     LINK-BUMON
                                     LINK-TANCD
                                     LINK-DATE
                                     LINK-TIME
                                     LINK-TORICD
                                     LINK-SAKUBA
                                     LINK-NOUDT
                                     LINK-NFTENCD
                                     LINK-NFSHOCD
                                     LINK-SAKKBN
                                     LINK-SURKBN
                                     LINK-HEN-SAKUBA
                                     LINK-HEN-SURYO
                                     LINK-UPD-DATE
                                     LINK-UPD-TIME
                                     LINK-KOUSIN.
**************************************************************
 DECLARATIVES.
 JOH-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFJOHOF.
     DISPLAY     JOH-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     JOH-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 HEN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SKSRHEF.
     DISPLAY     HEN-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     HEN-ST    UPON      CONS.
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
     ACCEPT    WK-TIME          FROM   TIME.
     MOVE      WK-TIME(1:6)       TO   WK-TIME6.
***   DISPLAY " LINK-SAKKBN = "  LINK-SAKKBN UPON CONS.
***   DISPLAY " LINK-SURKBN = "  LINK-SURKBN UPON CONS.
***   DISPLAY " LINK-HEN-SAKUBA = " LINK-HEN-SAKUBA UPON CONS.
*ファイルのＯＰＥＮ
     OPEN     INPUT     NFJOHOF.
     OPEN     I-O       SKSRHEF.
*ワークの初期化
     INITIALIZE         FLG-AREA.
*ナフコ基本情報ファイルスタート
     MOVE     SPACE               TO   JOH-REC.
     INITIALIZE                        JOH-REC.
     MOVE     LINK-DATE           TO   JOH-F02.
     MOVE     LINK-TIME           TO   JOH-F03.
     MOVE     LINK-TORICD         TO   JOH-F04.
     MOVE     LINK-SAKUBA         TO   JOH-F05.
     MOVE     LINK-NOUDT          TO   JOH-F09.
     MOVE     LINK-NFTENCD        TO   JOH-F06.
     MOVE     LINK-NFSHOCD        TO   JOH-F13.
     START  NFJOHOF  KEY  >=   JOH-F02  JOH-F03  JOH-F04
                               JOH-F05  JOH-F09  JOH-F06
                               JOH-F13
            INVALID
            MOVE  "END"           TO   END-FLG
            DISPLAY NC"＃対象データ無し１＃" UPON CONS
            GO                    TO   INIT-EXIT
     END-START.
*ナフコ基本情報ファイル読込
     PERFORM  NFJOHOF-READ-SEC.
*
     IF   END-FLG =  "END"
          DISPLAY NC"＃対象データ無し２＃" UPON CONS
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*    ナフコ基本情報ファイル読込
****************************************************************
 NFJOHOF-READ-SEC      SECTION.
     MOVE "NFJOHOF-READ-SEC" TO   S-NAME.
*
     READ  NFJOHOF  NEXT  AT  END
           MOVE   "END"      TO   END-FLG
           GO                TO   NFJOHOF-READ-EXIT
     END-READ.
*バッチ番号のチェック
     IF    LINK-DATE     =  JOH-F02
     AND   LINK-TIME     =  JOH-F03
     AND   LINK-TORICD   =  JOH-F04
           CONTINUE
     ELSE
           MOVE   "END"      TO   END-FLG
           GO                TO   NFJOHOF-READ-EXIT
     END-IF.
*作場チェック
     IF    LINK-SAKUBA   NOT =  SPACE
           IF    LINK-SAKUBA  =   JOH-F05
                 CONTINUE
           ELSE
                 MOVE "END"  TO   END-FLG
                 GO          TO   NFJOHOF-READ-EXIT
           END-IF
     END-IF.
*店舗チェック
     IF    LINK-NFTENCD  NOT =  ZERO
           IF    LINK-NFTENCD =   JOH-F06
                 CONTINUE
           ELSE
                 GO          TO   NFJOHOF-READ-SEC
           END-IF
     END-IF.
*ナフコ商品チェック
     IF    LINK-NFSHOCD  NOT =  SPACE
           IF    LINK-NFSHOCD =   JOH-F13
                 CONTINUE
           ELSE
                 GO          TO   NFJOHOF-READ-SEC
           END-IF
     END-IF.
*納品日チェック
     IF    LINK-NOUDT    NOT =  ZERO
           IF    LINK-NOUDT   =   JOH-F09
                 CONTINUE
           ELSE
                 GO          TO   NFJOHOF-READ-SEC
           END-IF
     END-IF.
*発注確定区分＝１（確定済）の場合は対象としない。
     IF    JOH-F34  =  "1"
           GO                TO   NFJOHOF-READ-SEC
     END-IF.
*
 NFJOHOF-READ-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             2.0
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*レコード初期化
     MOVE      SPACE              TO   HEN-REC.
     INITIALIZE                        HEN-REC.
*変更日付
     MOVE      LINK-UPD-DATE      TO   HEN-F01.
*変更時刻
     MOVE      LINK-UPD-TIME      TO   HEN-F02.
*変更担当者部門ＣＤ
     MOVE      LINK-BUMON         TO   HEN-F03.
*変更担当者ＣＤ
     MOVE      LINK-TANCD         TO   HEN-F04.
*管理番号
     MOVE      JOH-F01            TO   HEN-F05.
*バッチ日付
     MOVE      JOH-F02            TO   HEN-F06.
*バッチ時間
     MOVE      JOH-F03            TO   HEN-F07.
*バッチ取引先
     MOVE      JOH-F04            TO   HEN-F08.
*倉庫ＣＤ
     MOVE      JOH-F05            TO   HEN-F09.
*店舗ＣＤ
     MOVE      JOH-F06            TO   HEN-F10.
*納品日
     MOVE      JOH-F09            TO   HEN-F11.
*伝票番号
     MOVE      JOH-F07            TO   HEN-F12.
*行番号
     MOVE      JOH-F08            TO   HEN-F13.
*相手商品ＣＤ
     MOVE      JOH-F13            TO   HEN-F14.
*相手ＪＡＮＣＤ
     MOVE      JOH-F14            TO   HEN-F15.
*発注単位
     MOVE      JOH-F18            TO   HEN-F16.
*発注数
     MOVE      JOH-F19            TO   HEN-F17.
*訂正済数
     MOVE      JOH-F20            TO   HEN-F18.
*作場変更区分
     MOVE      LINK-SAKKBN        TO   HEN-F19.
*数量変更区分
     MOVE      LINK-SURKBN        TO   HEN-F20.
*訂正数
     IF   LINK-SURKBN  =  "1"
          MOVE LINK-HEN-SURYO     TO   HEN-F21
     END-IF.
*変更作場
     IF   LINK-SAKKBN  =  "1"
          MOVE LINK-HEN-SAKUBA    TO   HEN-F22
     END-IF.
*更新区分
*****MOVE      "1"                TO   HEN-F99.
*出力
     WRITE     HEN-REC.
     ADD       1                  TO   WT-CNT.
*ナフコ基本情報ファイル読込
     PERFORM   NFJOHOF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE              NFJOHOF  SKSRHEF.
*
     DISPLAY NC"＃抽出件数＝" WT-CNT  UPON CONS.
*
     IF   WT-CNT  =  0
          MOVE   "1"          TO      LINK-KOUSIN
     ELSE
          MOVE   "0"          TO      LINK-KOUSIN
     END-IF.
*
 END-EXIT.
     EXIT.
*****************<<  SSY3881B   END PROGRAM  >>******************
