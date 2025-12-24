****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ出荷支援システム　　　　　　*
*    業務名　　　　　　　：　作場変更、出荷数変更　　　        *
*    モジュール名　　　　：　ナフコ作場・数量一括変更制御　　　*
*    作成日／作成者　　　：　2019/02/19 NAV INOUE              *
*    処理概要　　　　　　：　一括変更データよりデータ更新。　　*
*                          　サブルーチンをＣＡＬＬし更新。　　*
*                          　ナフコ基本情報、売上伝票、在庫Ｍ　*
*<履歴>*********************************************************
* XXXX/XX/XX XXXXXXXXX ＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮ
* 2019/02/19 INOUE　　 新規作成
*　　　　　　　　　　　　　　　　　　　　　　　　　　　　　
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSY5160B.
 AUTHOR.               NAV.
 DATE-WRITTEN.         2019/02/19.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*作場数量一括変更データ
     SELECT  CHGXXXT1  ASSIGN    TO        CHGXXXT1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       CHG-F0501
                                           CHG-F05021
                                           CHG-F05022
                                           CHG-F05023
                                           CHG-F0503
                                           CHG-F0504
                                           CHG-F0505
                                           CHG-F0506
                                           CHG-F060
                       FILE      STATUS    CHG-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 作場数量一括変更データ
****************************************************************
 FD  CHGXXXT1
                       LABEL     RECORD    IS   STANDARD.
                       COPY      CHGXXXT1  OF   XFDLIB
                       JOINING   CHG       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  CHG-ST                   PIC  X(02).
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
     03  CHG-ERR           PIC N(20) VALUE
                        NC"作場数量一括変更データエラー".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
 01  LINK-SUB-IN.
     03  LINK-SUB-BUMON    PIC X(04).
     03  LINK-SUB-TANCD    PIC X(02).
     03  LINK-SUB-KANRI    PIC 9(08).
     03  LINK-SUB-BTDATE   PIC 9(08).
     03  LINK-SUB-BTTIME   PIC 9(04).
     03  LINK-SUB-BTTOKC   PIC 9(08).
     03  LINK-SUB-SAKUBA   PIC X(02).
     03  LINK-SUB-NOUDT    PIC 9(08).
     03  LINK-SUB-TENCD    PIC 9(05).
     03  LINK-SUB-NFSHOCD  PIC X(08).
     03  LINK-SUB-DENNO    PIC 9(09).
     03  LINK-SUB-GYO      PIC 9(02).
     03  LINK-SUB-SAKKBN   PIC X(01).
     03  LINK-SUB-SURKBN   PIC X(01).
     03  LINK-SUB-HENSAKB  PIC X(02).
     03  LINK-SUB-HENSURY  PIC 9(05).
     03  LINK-SUB-KIHDATE  PIC 9(08).
     03  LINK-SUB-KIHTIME  PIC 9(06).
 01  LINK-SUB-OUT.
     03  LINK-SUB-KOUSIN1  PIC X(01).
     03  LINK-SUB-KOUSIN2  PIC X(01).
*
******************************************************************
 LINKAGE           SECTION.
******************************************************************
 01  LINK-BUMON          PIC  X(04).
 01  LINK-TANCD          PIC  X(02).
*01  LINK-UPD-DATE       PIC  9(08).
*01  LINK-UPD-TIME       PIC  9(06).
*01  LINK-KOUSIN-KENSU   PIC  9(07).
*
**************************************************************
 PROCEDURE       DIVISION  USING     LINK-BUMON
                                     LINK-TANCD.
*                                    LINK-UPD-DATE
*                                    LINK-UPD-TIME
*                                    LINK-KOUSIN-KENSU.
**************************************************************
 DECLARATIVES.
 CHG-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE CHGXXXT1.
     DISPLAY     CHG-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     CHG-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS START"     TO   S-NAME.
***
     DISPLAY  "*** SSY5160B START ***"   UPON CONS.
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC          UNTIL   END-FLG   =   "END".
     PERFORM   END-SEC.
***
     STOP  RUN.
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
*
*****DISPLAY "LINK-UPD-DATE = " LINK-UPD-DATE  UPON CONS.
*****DISPLAY "LINK-UPD-TIME = " LINK-UPD-TIME  UPON CONS.
*ファイルのＯＰＥＮ
     OPEN     I-O       CHGXXXT1.
*ワークの初期化
     INITIALIZE         FLG-AREA.
*    MOVE     ZERO                TO   LINK-KOUSIN-KENSU.
*作場数量一括変更データスタート
*    MOVE     SPACE               TO   CHG-REC.
*    INITIALIZE                        CHG-REC.
*    MOVE     LINK-UPD-DATE       TO   CHG-F01.
*    MOVE     LINK-UPD-TIME       TO   CHG-F02.
*    START  CHGXXXT1  KEY  >=   CHG-F01  CHG-F02  CHG-F03
*                              CHG-F04  CHG-F05  CHG-F09
*                              CHG-F10  CHG-F11  CHG-F12
*                              CHG-F13
*           INVALID
*           MOVE  "END"           TO   END-FLG
*           DISPLAY NC"＃対象データ無し１＃" UPON CONS
*           GO                    TO   INIT-EXIT
*    END-START.
*作場数量一括変更データ読込
     PERFORM  CHGXXXT1-READ-SEC.
*
     IF   END-FLG =  "END"
          DISPLAY NC"＃対象データ無し＃" UPON CONS
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*    ナフコ基本情報ファイル読込
****************************************************************
 CHGXXXT1-READ-SEC      SECTION.
     MOVE "CHGXXXT1-READ-SEC" TO   S-NAME.
*
     READ  CHGXXXT1  NEXT  AT  END
           MOVE   "END"      TO   END-FLG
           GO                TO   CHGXXXT1-READ-EXIT
           NOT  AT  END
           ADD     1         TO   READ-CNT
     END-READ.
*
     IF    READ-CNT(5:3)  =  "000" OR "500"
           DISPLAY "#READ-CNT = " READ-CNT UPON CONS
     END-IF.
*バッチ番号のチェック
*    IF    LINK-UPD-DATE =  CHG-F01
*    AND   LINK-UPD-TIME =  CHG-F02
*          CONTINUE
*    ELSE
*          MOVE   "END"      TO   END-FLG
*          GO                TO   NFJOHOF-READ-EXIT
*    END-IF.
*
 CHGXXXT1-READ-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             2.0
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*パラメタ初期化
     INITIALIZE                        LINK-SUB-IN.
     INITIALIZE                        LINK-SUB-OUT.
*パラメタセット
*　部門
     MOVE      LINK-BUMON         TO   LINK-SUB-BUMON.
*　担当者ＣＤ
     MOVE      LINK-TANCD         TO   LINK-SUB-TANCD.
*　管理番号
     MOVE      CHG-F0501          TO   LINK-SUB-KANRI.
*　バッチ日付
     MOVE      CHG-F05021(1:8)    TO   LINK-SUB-BTDATE.
*　バッチ時刻
     MOVE      CHG-F05022(1:4)    TO   LINK-SUB-BTTIME.
*　バッチ取引先
     MOVE      CHG-F05023         TO   LINK-SUB-BTTOKC.
*　作場ＣＤ
     MOVE      CHG-F0509          TO   LINK-SUB-SAKUBA.
*　納品日
     MOVE      CHG-F0503(1:8)     TO   LINK-SUB-NOUDT.
*　店舗ＣＤ
     MOVE      CHG-F0504          TO   LINK-SUB-TENCD.
*　ナフコ商品ＣＤ
     MOVE      SPACE              TO   LINK-SUB-NFSHOCD.
*　伝票番号
     MOVE      CHG-F0505          TO   LINK-SUB-DENNO.
*　行番号
     MOVE      CHG-F0506          TO   LINK-SUB-GYO.
*　作場変更区分
     IF        CHG-F0509 NOT = CHG-F050A
               MOVE      "1"      TO   LINK-SUB-SAKKBN
     END-IF.
*　数量変更区分
     IF        CHG-F0507 NOT = CHG-F0508
               MOVE      "1"      TO   LINK-SUB-SURKBN
     END-IF.
*　変更作場
     MOVE      CHG-F050A          TO   LINK-SUB-HENSAKB.
*　数量変更
     MOVE      CHG-F0508          TO   LINK-SUB-HENSURY.
*　基本日付
     MOVE      ZERO               TO   LINK-SUB-KIHDATE.
*　基本時刻
     MOVE      ZERO               TO   LINK-SUB-KIHTIME.
*T↓
*    DISPLAY "LINK-SUB-IN  = " LINK-SUB-IN   UPON CONS.
*T↑
*　サブルーチンコール
     CALL     "SSY3883B"       USING   LINK-SUB-IN
                                       LINK-SUB-OUT.
     ADD       1                  TO   WT-CNT.
*****DISPLAY "LINK-SUB-OUT = " LINK-SUB-OUT  UPON CONS.
*  更新確認
     IF    LINK-SUB-KOUSIN1  = "1"
     AND   LINK-SUB-KOUSIN2  = "1"
           MOVE    "1"            TO   CHG-F07
           REWRITE  CHG-REC
     END-IF.
*
*ナフコ基本情報ファイル読込
     PERFORM   CHGXXXT1-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             CHGXXXT1.
*
     DISPLAY NC"＃更新起動件数＝" WT-CNT  UPON CONS.
*
 END-EXIT.
     EXIT.
*****************<<  SSY5160B   END PROGRAM  >>******************
