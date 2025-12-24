# SSY9307B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SSY9307B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＥＤＩ　出荷　　　　　　　        *
*    サブシステム　　　　：　ＡＭＡＺＯＮ　ＥＤＩ　　　　　　　*
*    モジュール名　　　　：　出荷確定データ更新制御　　　　　　*
*    作成日／作成者　　　：　2020/11/16 NAV INOUE              *
*    処理概要　　　　　　：　出荷確定ファイルよりデータ更新。　*
*                          　サブルーチンをＣＡＬＬし更新。　　*
*                          　（基本情報、売上伝票、在庫Ｍ）　　*
*<履歴>*********************************************************
*    更新日／更新者　　　：　2022.01.08 NAV INOUE
*    更新概要　　　　　　：　数量変更判定ロジック変更
*　　　　　　　　　　　　　　（売上伝票ファイル追加）　
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSY9307B.
*                 流用:SSY5160B.
 AUTHOR.               NAV.
 DATE-WRITTEN.         2020/11/16.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*ＡＭＡＺＯＮ出荷確定ファイル
     SELECT  AMZSYKL1  ASSIGN    TO        AMZSYKL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       SYK-FE01
                                           SYK-FE02
                                           SYK-FE03
                                           SYK-FB09
                                           SYK-FB11
                                           SYK-FA04
                                           SYK-FB01
                                           SYK-FB02
                       FILE      STATUS    SYK-ST.
*
*↓2021.01.18
*売上伝票ファイル
     SELECT   SHTDENLA ASSIGN     DA-01-VI-SHTDENLA
                       ORGANIZATION        INDEXED
                       ACCESS     MODE     RANDOM
                       RECORD     KEY      DEN-F46
                                           DEN-F47
                                           DEN-F01
                                           DEN-F48
                                           DEN-F02
                                           DEN-F04
                                           DEN-F051
                                           DEN-F07
                                           DEN-F112
                                           DEN-F03
                        FILE      STATUS   DEN-ST.
*↑2021.01.18
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = ＡＭＡＺＯＮ出荷確定ファイル
****************************************************************
 FD  AMZSYKL1
                       LABEL     RECORD    IS   STANDARD.
                       COPY      AMZSYKL1  OF   XFDLIB
                       JOINING   SYK       AS   PREFIX.
*
*↓2021.01.18
****************************************************************
*    FILE = 売上伝票ファイル
****************************************************************
 FD  SHTDENLA           LABEL    RECORD    IS   STANDARD.
                        COPY     SHTDENF   OF   XFDLIB
                        JOINING  DEN       AS   PREFIX.
*↑2021.01.18
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  SYK-ST                   PIC  X(02).
*↓2021.01.18
     03  DEN-ST                   PIC  X(02).
*↑2021.01.18
*フラグ領域
 01  FLG-AREA.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  READ-CNT                 PIC  9(07)  VALUE  ZERO.
     03  WT-CNT                   PIC  9(07)  VALUE  ZERO.
*↓2021.01.18
     03  SHTDENLA-INV-FLG         PIC  X(03)  VALUE  ZERO.
*↑2021.01.18
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
     03  SYK-ERR                  PIC N(20) VALUE
                        NC"ＡＭＡＺＯＮ出荷確定ファイルエラー".
*↓2021.01.18
     03  DEN-ERR                  PIC N(20) VALUE
                        NC"売上伝票ファイルエラー".
*↑2021.01.18
*
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
     03  LINK-SUB-BTDATE   PIC 9(08).
     03  LINK-SUB-BTTIME   PIC 9(04).
     03  LINK-SUB-BTTOKC   PIC 9(08).
     03  LINK-SUB-SOKOCD   PIC X(02).
     03  LINK-SUB-DENNO    PIC 9(09).
     03  LINK-SUB-GYO      PIC 9(02).
     03  LINK-SUB-TENCD    PIC 9(05).
     03  LINK-SUB-HACDT    PIC 9(08).
     03  LINK-SUB-HACNO    PIC X(10).
     03  LINK-SUB-HACGYO   PIC 9(03).
     03  LINK-SUB-IDCD     PIC X(10).
     03  LINK-SUB-HINCD    PIC X(20).
     03  LINK-SUB-SYKDT    PIC 9(08).
     03  LINK-SUB-NOUDT    PIC 9(08).
     03  LINK-SUB-KONPO    PIC 9(07).
     03  LINK-SUB-JUURYO   PIC 9(10).
     03  LINK-SUB-SSCCCD   PIC X(48).
     03  LINK-SUB-SURKBN   PIC X(01).
     03  LINK-SUB-KENSURY  PIC 9(10).
 01  LINK-SUB-OUT.
     03  LINK-SUB-PGDATE   PIC 9(08).
     03  LINK-SUB-PGTIME   PIC 9(06).
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
 SYK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE AMZSYKL1.
     DISPLAY     SYK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SYK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
*↓2021.01.18
 DEN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SHTDENLA.
     DISPLAY     DEN-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     DEN-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
*↑2021.01.18
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS START"     TO   S-NAME.
***
     DISPLAY  "*** SSY9307B START ***"   UPON CONS.
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
     OPEN     I-O       AMZSYKL1.
*↓2021.01.18
     OPEN     INPUT     SHTDENLA.
*↑2021.01.18
*ワークの初期化
     INITIALIZE         FLG-AREA.
*
*ＡＭＡＺＯＮ出荷確定ファイル読込
     PERFORM  AMZSYKL1-READ-SEC.
*
     IF   END-FLG =  "END"
          DISPLAY NC"＃対象データ無し＃" UPON CONS
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*    ＡＭＺ基本情報ファイル読込
****************************************************************
 AMZSYKL1-READ-SEC      SECTION.
     MOVE "AMZSYKL1-READ-SEC" TO   S-NAME.
*
     READ  AMZSYKL1  NEXT  AT  END
           MOVE   "END"      TO   END-FLG
           GO                TO   AMZSYKL1-READ-EXIT
           NOT  AT  END
           ADD     1         TO   READ-CNT
     END-READ.
*
     IF    READ-CNT(5:3)  =  "000" OR "500"
           DISPLAY "#READ-CNT = " READ-CNT UPON CONS
     END-IF.
*確定(日付)(時刻)チェック
     IF    SYK-FE14      =  ZERO
     AND   SYK-FE15      =  ZERO
           CONTINUE
     ELSE
           GO                TO   AMZSYKL1-READ-SEC
     END-IF.
*
 AMZSYKL1-READ-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             2.0
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*パラメタ初期化
     INITIALIZE                        LINK-SUB-IN.
     INITIALIZE                        LINK-SUB-OUT.
*↓2021.01.18
*売上伝票ファイル検索
     MOVE     SYK-FE01             TO   DEN-F46.
     MOVE     SYK-FE02             TO   DEN-F47.
     MOVE     SYK-FE03             TO   DEN-F01.
     MOVE     SYK-FB09             TO   DEN-F48.
     MOVE     SYK-FB01             TO   DEN-F02.
     MOVE     0                    TO   DEN-F04.
     MOVE     40                   TO   DEN-F051.
     MOVE     SYK-FB11             TO   DEN-F07.
     MOVE     SYK-FA04             TO   DEN-F112.
     MOVE     SYK-FB02             TO   DEN-F03.
     PERFORM  SHTDENLA-READ-SEC.
     IF       SHTDENLA-INV-FLG  =  "INV"
              DISPLAY NC"売上伝票Ｆなし！？" UPON CONS
     END-IF.
*↑2021.01.18
*
*パラメタセット
*　部門
     MOVE      LINK-BUMON         TO   LINK-SUB-BUMON.
*　担当者ＣＤ
     MOVE      LINK-TANCD         TO   LINK-SUB-TANCD.
*　バッチ日付
     MOVE      SYK-FE01           TO   LINK-SUB-BTDATE.
*　バッチ時刻
     MOVE      SYK-FE02           TO   LINK-SUB-BTTIME.
*　バッチ取引先
     MOVE      SYK-FE03           TO   LINK-SUB-BTTOKC.
*　倉庫ＣＤ
     MOVE      SYK-FB09           TO   LINK-SUB-SOKOCD.
*　伝票番号
     MOVE      SYK-FB01           TO   LINK-SUB-DENNO.
*　行番号
     MOVE      SYK-FB02           TO   LINK-SUB-GYO.
*　店舗ＣＤ
     MOVE      SYK-FB11           TO   LINK-SUB-TENCD.
*　発注日
     MOVE      SYK-FA04           TO   LINK-SUB-HACDT.
*　発注書番号
     MOVE      SYK-FA03           TO   LINK-SUB-HACNO.
*　明細行番号
     MOVE      SYK-FA09           TO   LINK-SUB-HACGYO.
*　ＩＤコード
     MOVE      SYK-FA08           TO   LINK-SUB-IDCD.
*　品番
     MOVE      SYK-FA14           TO   LINK-SUB-HINCD.
*　出荷日
     MOVE      SYK-FC03           TO   LINK-SUB-SYKDT.
*　納品日
     MOVE      SYK-FE18           TO   LINK-SUB-NOUDT.
*　梱包数
     MOVE      SYK-FC01           TO   LINK-SUB-KONPO.
*　重量
     MOVE      SYK-FC02           TO   LINK-SUB-JUURYO.
*　SSCCコード番号
     MOVE      SYK-FC04           TO   LINK-SUB-SSCCCD.
*　数量変更区分
*↓2021.01.18
*  --IF        SYK-FB04   =    SYK-FC05
*  --          MOVE      " "      TO   LINK-SUB-SURKBN
*  --ELSE
*  --          MOVE      "1"      TO   LINK-SUB-SURKBN
*  --END-IF.
     IF        SHTDENLA-INV-FLG    =  "INV"
               MOVE      " "      TO   LINK-SUB-SURKBN
     ELSE
               IF        DEN-F15   =   SYK-FC05
                         MOVE     " "  TO   LINK-SUB-SURKBN
               ELSE
                         MOVE     "1"  TO   LINK-SUB-SURKBN
               END-IF
     END-IF.
*↑2021.01.18
*
*　出荷検品数量
     MOVE      SYK-FC05           TO   LINK-SUB-KENSURY.
*T↓
*    DISPLAY "LINK-SUB-BUMON  = " LINK-SUB-BUMON   UPON CONS.
*    DISPLAY "LINK-SUB-TANCD  = " LINK-SUB-TANCD   UPON CONS.
*    DISPLAY "LINK-SUB-BTDATE = " LINK-SUB-BTDATE  UPON CONS.
*    DISPLAY "LINK-SUB-BTTIME = " LINK-SUB-BTTIME  UPON CONS.
*    DISPLAY "LINK-SUB-BTTOKC = " LINK-SUB-BTTOKC  UPON CONS.
*    DISPLAY "LINK-SUB-SOKOCD = " LINK-SUB-SOKOCD  UPON CONS.
*    DISPLAY "LINK-SUB-DENNO  = " LINK-SUB-DENNO   UPON CONS.
*    DISPLAY "LINK-SUB-GYO    = " LINK-SUB-GYO     UPON CONS.
*    DISPLAY "LINK-SUB-TENCD  = " LINK-SUB-TENCD   UPON CONS.
*    DISPLAY "LINK-SUB-HACDT  = " LINK-SUB-HACDT   UPON CONS.
*    DISPLAY "LINK-SUB-HACNO  = " LINK-SUB-HACNO   UPON CONS.
*    DISPLAY "LINK-SUB-HACGYO = " LINK-SUB-HACGYO  UPON CONS.
*    DISPLAY "LINK-SUB-IDCD   = " LINK-SUB-IDCD    UPON CONS.
*    DISPLAY "LINK-SUB-HINCD  = " LINK-SUB-HINCD   UPON CONS.
*    DISPLAY "LINK-SUB-SYKDT  = " LINK-SUB-SYKDT   UPON CONS.
*    DISPLAY "LINK-SUB-NOUDT  = " LINK-SUB-NOUDT   UPON CONS.
*    DISPLAY "LINK-SUB-KONPO  = " LINK-SUB-KONPO   UPON CONS.
*    DISPLAY "LINK-SUB-JUURYO = " LINK-SUB-JUURYO  UPON CONS.
*    DISPLAY "LINK-SUB-SSCCCD = " LINK-SUB-SSCCCD  UPON CONS.
*    DISPLAY "LINK-SUB-SURKBN = " LINK-SUB-SURKBN  UPON CONS.
*    DISPLAY "LINK-SUB-KENSURY= " LINK-SUB-KENSURY UPON CONS.
*T↑
*　サブルーチンコール
     CALL     "SSY9308B"       USING   LINK-SUB-IN
                                       LINK-SUB-OUT.
     ADD       1                  TO   WT-CNT.
*
*T↓
*    DISPLAY "LINK-SUB-PGDATE = " LINK-SUB-PGDATE  UPON CONS.
*    DISPLAY "LINK-SUB-PGTIME = " LINK-SUB-PGTIME  UPON CONS.
*    DISPLAY "LINK-SUB-KOUSIN1= " LINK-SUB-KOUSIN1 UPON CONS.
*    DISPLAY "LINK-SUB-KOUSIN2= " LINK-SUB-KOUSIN2 UPON CONS.
*T↑
*  更新確認
     IF    LINK-SUB-KOUSIN1  = "1"
     AND   LINK-SUB-KOUSIN2  = "1"
*　        確定（日付）
           MOVE      LINK-SUB-PGDATE         TO   SYK-FE14
*　        確定（時刻）
           MOVE      LINK-SUB-PGTIME         TO   SYK-FE15
*　
           REWRITE   SYK-REC
     END-IF.
*
*ＡＭＺ基本情報ファイル読込
     PERFORM   AMZSYKL1-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*    売上伝票ファイル　検索
****************************************************************
 SHTDENLA-READ-SEC      SECTION.
     MOVE    "SHTDENLA-READ-SEC"  TO   S-NAME.
*
     READ  SHTDENLA     INVALID
                        MOVE "INV" TO  SHTDENLA-INV-FLG
                   NOT  INVALID
                        MOVE SPACE TO  SHTDENLA-INV-FLG
     END-READ.
*
 SHTDENL1-READ-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             AMZSYKL1.
*
     DISPLAY NC"＃更新起動件数＝" WT-CNT  UPON CONS.
*
 END-EXIT.
     EXIT.
*****************<<  SSY9307B   END PROGRAM  >>******************

```
