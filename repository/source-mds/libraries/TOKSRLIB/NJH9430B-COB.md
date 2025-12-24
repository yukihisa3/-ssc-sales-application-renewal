# NJH9430B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NJH9430B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＥＤＩ　出荷　　　　　　　        *
*    サブシステム　　　　：　ヨドバシ　ＥＤＩ　　　　　　　　　*
*    モジュール名　　　　：　受注データ変換（発注変更）　　　　*
*    作成日／作成者　　　：　2021/07/30 NAV INOUE              *
*    処理概要　　　　　　：　取込ワーク（発注変更）より更新　　*
*                          　サブルーチンをＣＡＬＬ　　　　　　*
*                          　（基本情報、売上伝票、在庫Ｍ）　　*
*<履歴>*********************************************************
*    更新日／更新者　　　：　2022/07/13 NAV TAKAHASHI
*    更新概要　　　　　　：　キャンセル判断変更
*　　　　　　　　　　　　　　　　　　　　　　　　　　　　　
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           NJH9430B.
*                 流用:SSY9307B.
 AUTHOR.               NAV.
 DATE-WRITTEN.         2021/07/30.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*ヨドバシ取込ワーク（発注変更）
     SELECT  YODCHGL1  ASSIGN    TO        YODCHGL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       CHG-F03
                                           CHG-F02
                                           CHG-F07
                       FILE      STATUS    CHG-ST.
*
*基本情報ファイル
     SELECT  YODJOHL1  ASSIGN    TO        YODJOHL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       JOH-F17
                                           JOH-F03
                                           JOH-F02
                                           JOH-F07
                       FILE      STATUS    JOH-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = ヨドバシ取込ワーク（発注変更）
****************************************************************
 FD  YODCHGL1
                       LABEL     RECORD    IS   STANDARD.
                       COPY      YODCHGL1  OF   XFDLIB
                       JOINING   CHG       AS   PREFIX.
*
****************************************************************
*    FILE = 基本情報ファイル
****************************************************************
 FD  YODJOHL1
                       LABEL     RECORD    IS   STANDARD.
                       COPY      YODJOHL1  OF   XFDLIB
                       JOINING   JOH       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  CHG-ST                   PIC  X(02).
     03  JOH-ST                   PIC  X(02).
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
     03  CHG-ERR                  PIC N(20) VALUE
                        NC"取込ワーク（発注変更）エラー".
     03  JOH-ERR                  PIC N(20) VALUE
                        NC"基本情報ファイル　　　エラー".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
*店舗コード
 01  WK-TEN-F011.
     03  WK-TEN-F011-1      PIC   9(01)  VALUE  ZERO.
     03  WK-TEN-F011-2      PIC   9(04)  VALUE  ZERO.
 01  WK-TEN-F011-R REDEFINES WK-TEN-F011.
     03  WK-TEN-F011-RR     PIC   9(05).
*
 01  LINK-SUB-IN.
     03  LINK-SUB-BUMON    PIC X(04).
     03  LINK-SUB-TANCD    PIC X(02).
     03  LINK-SUB-BTDATE   PIC 9(08).
     03  LINK-SUB-BTTIME   PIC 9(04).
     03  LINK-SUB-BTTOKC   PIC 9(08).
*    03  LINK-SUB-SOKOCD   PIC X(02).
     03  LINK-SUB-DENNO    PIC 9(09).
     03  LINK-SUB-GYO      PIC 9(02).
     03  LINK-SUB-TENCD    PIC 9(05).
     03  LINK-SUB-TYPE     PIC X(06).
     03  LINK-SUB-CANCEL   PIC X(70).
     03  LINK-SUB-HACDT    PIC 9(08).
     03  LINK-SUB-HACNO    PIC 9(10).
     03  LINK-SUB-HACGYO   PIC 9(06).
*    03  LINK-SUB-IDCD     PIC X(10).
*    03  LINK-SUB-HINCD    PIC X(20).
*    03  LINK-SUB-CHGDT    PIC 9(08).
     03  LINK-SUB-TANKA    PIC 9(15).
     03  LINK-SUB-SURYO    PIC 9(15).
     03  LINK-SUB-NOUDT    PIC 9(08).
*    03  LINK-SUB-KONPO    PIC 9(07).
*    03  LINK-SUB-JUURYO   PIC 9(10).
*    03  LINK-SUB-SSCCCD   PIC X(48).
*    03  LINK-SUB-SURKBN   PIC X(01).
*    03  LINK-SUB-KENSURY  PIC 9(10).
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
 CHG-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE YODCHGL1.
     DISPLAY     CHG-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     CHG-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 JOH-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE YODJOHL1.
     DISPLAY     JOH-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     JOH-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS START"     TO   S-NAME.
***
     DISPLAY  "*** NJH9430B START ***"   UPON CONS.
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
     OPEN     I-O       YODCHGL1.
     OPEN     INPUT     YODJOHL1.
*ワークの初期化
     INITIALIZE         FLG-AREA.
*
*ヨドバシ取込ワーク（発注変更）読込
     PERFORM  YODCHGL1-READ-SEC.
*
     IF   END-FLG =  "END"
          DISPLAY NC"＃対象データ無し＃" UPON CONS
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*    取込ワーク（発注変更）読込
****************************************************************
 YODCHGL1-READ-SEC      SECTION.
     MOVE "YODCHGL1-READ-SEC" TO   S-NAME.
*
     READ  YODCHGL1  NEXT  AT  END
           MOVE   "END"      TO   END-FLG
           GO                TO   YODCHGL1-READ-EXIT
           NOT  AT  END
           ADD     1         TO   READ-CNT
     END-READ.
*
     IF    READ-CNT(5:3)  =  "000" OR "500"
           DISPLAY "#READ-CNT = " READ-CNT UPON CONS
     END-IF.
*更新済ＦＬＧチェック
     IF    CHG-F23      =  " "
           CONTINUE
     ELSE
           GO                TO   YODCHGL1-READ-SEC
     END-IF.
*
 YODCHGL1-READ-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             2.0
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*パラメタ初期化
     INITIALIZE                        LINK-SUB-IN.
     INITIALIZE                        LINK-SUB-OUT.
*基本情報ファイル存在チェック
     MOVE      CHG-F17            TO   JOH-F17.
     MOVE      CHG-F03            TO   JOH-F03.
     MOVE      CHG-F02            TO   JOH-F02.
     MOVE      CHG-F07            TO   JOH-F07.
     READ      YODJOHL1
         INVALID
               DISPLAY NC"基本情報ファイルなし！" UPON CONS
               DISPLAY NC"取引先ＣＤ＝" CHG-F17   UPON CONS
               DISPLAY NC"発注日　　＝" CHG-F03   UPON CONS
               DISPLAY NC"発注番号　＝" CHG-F02   UPON CONS
               DISPLAY NC"発注行　　＝" CHG-F07   UPON CONS
               GO                   TO  MAIN-010
     END-READ.
*T
*    DISPLAY NC"取引先ＣＤ＝" JOH-F17   UPON CONS.
*    DISPLAY NC"発注日　　＝" JOH-F03   UPON CONS.
*    DISPLAY NC"発注番号　＝" JOH-F02   UPON CONS.
*    DISPLAY NC"発注行　　＝" JOH-F07   UPON CONS.
*    DISPLAY NC"基幹伝番　＝" JOH-F18   UPON CONS.
*    DISPLAY NC"基幹行番　＝" JOH-F19   UPON CONS.
*T
*パラメタセット
*　部門
     MOVE      LINK-BUMON         TO   LINK-SUB-BUMON.
*　担当者ＣＤ
     MOVE      LINK-TANCD         TO   LINK-SUB-TANCD.
*　バッチ日付
     MOVE      CHG-F15            TO   LINK-SUB-BTDATE.
*　バッチ時刻
     MOVE      CHG-F16            TO   LINK-SUB-BTTIME.
*　バッチ取引先
     MOVE      CHG-F17            TO   LINK-SUB-BTTOKC.
*　倉庫ＣＤ
*    MOVE      CHG-F18            TO   LINK-SUB-SOKOCD.
*　伝票番号
*    MOVE      CHG-F19            TO   LINK-SUB-DENNO.
     MOVE      JOH-F18            TO   LINK-SUB-DENNO.
*T
*    DISPLAY NC"基幹伝票番号＝"   LINK-SUB-DENNO   UPON CONS.
*T
*　行番号
*    MOVE      CHG-F20            TO   LINK-SUB-GYO.
     MOVE      JOH-F19            TO   LINK-SUB-GYO.
*T
*    DISPLAY NC"基幹行　番号＝"   LINK-SUB-GYO     UPON CONS.
*T
*　店舗ＣＤ
     MOVE      CHG-F06(1:4)       TO   WK-TEN-F011-2.
     MOVE      WK-TEN-F011-RR     TO   LINK-SUB-TENCD.
*　メッセージタイプ
     MOVE      CHG-F01            TO   LINK-SUB-TYPE.
*　キャンセル区分
     MOVE      CHG-F09            TO   LINK-SUB-CANCEL.
*　発注伝票日付
     MOVE      CHG-F03            TO   LINK-SUB-HACDT.
*　購買発注番号
     MOVE      CHG-F02            TO   LINK-SUB-HACNO.
*　明細行番号
     MOVE      CHG-F07            TO   LINK-SUB-HACGYO.
*　ＩＤコード
*    MOVE      CHG-FA08           TO   LINK-SUB-IDCD.
*　品番
*    MOVE      CHG-FA14           TO   LINK-SUB-HINCD.
*　出荷日
*    MOVE      CHG-FC03           TO   LINK-SUB-CHGDT.
*　明細単価（変更項目）
     MOVE      CHG-F10            TO   LINK-SUB-TANKA.
*　発注数量（変更項目）
*#2022/07/13 NAV ST 明細フリーテキスト項目で判断
*# CHG-F09(1:9)="338370P03'の時、キャンセルと判断し数量０にする
*****IF        CHG-F09  NOT = SPACE
*              MOVE     0         TO   LINK-SUB-SURYO
*    ELSE
*              MOVE     CHG-F13   TO   LINK-SUB-SURYO
*****END-IF.
     IF        CHG-F09(1:9)  =  "338370P03"
               MOVE     0         TO   LINK-SUB-SURYO
     ELSE
               MOVE     CHG-F13   TO   LINK-SUB-SURYO
     END-IF.
*#2022/07/13 NAV ED 明細フリーテキスト項目で判断
*　納入期日（変更項目）
     MOVE      CHG-F14            TO   LINK-SUB-NOUDT.
*　梱包数
*    MOVE      CHG-FC01           TO   LINK-SUB-KONPO.
*　重量
*    MOVE      CHG-FC02           TO   LINK-SUB-JUURYO.
*　SSCCコード番号
*    MOVE      CHG-FC04           TO   LINK-SUB-SSCCCD.
*　数量変更区分
*    IF        CHG-FB04   =    CHG-FC05
*              MOVE      " "      TO   LINK-SUB-SURKBN
*    ELSE
*              MOVE      "1"      TO   LINK-SUB-SURKBN
*    END-IF.
*　出荷検品数量
*    MOVE      CHG-FC05           TO   LINK-SUB-KENSURY.
*T↓
*    DISPLAY "LINK-SUB-BUMON  = " LINK-SUB-BUMON   UPON CONS.
*    DISPLAY "LINK-SUB-TANCD  = " LINK-SUB-TANCD   UPON CONS.
*    DISPLAY "LINK-SUB-BTDATE = " LINK-SUB-BTDATE  UPON CONS.
*    DISPLAY "LINK-SUB-BTTIME = " LINK-SUB-BTTIME  UPON CONS.
*    DISPLAY "LINK-SUB-BTTOKC = " LINK-SUB-BTTOKC  UPON CONS.
*****DISPLAY "LINK-SUB-SOKOCD = " LINK-SUB-SOKOCD  UPON CONS.
*    DISPLAY "LINK-SUB-DENNO  = " LINK-SUB-DENNO   UPON CONS.
*    DISPLAY "LINK-SUB-GYO    = " LINK-SUB-GYO     UPON CONS.
*    DISPLAY "LINK-SUB-TENCD  = " LINK-SUB-TENCD   UPON CONS.
*    DISPLAY "LINK-SUB-TYPE   = " LINK-SUB-TYPE    UPON CONS.
*    DISPLAY "LINK-SUB-CANCEL = " LINK-SUB-CANCEL  UPON CONS.
*    DISPLAY "LINK-SUB-HACDT  = " LINK-SUB-HACDT   UPON CONS.
*    DISPLAY "LINK-SUB-HACNO  = " LINK-SUB-HACNO   UPON CONS.
*    DISPLAY "LINK-SUB-HACGYO = " LINK-SUB-HACGYO  UPON CONS.
*****DISPLAY "LINK-SUB-IDCD   = " LINK-SUB-IDCD    UPON CONS.
*****DISPLAY "LINK-SUB-HINCD  = " LINK-SUB-HINCD   UPON CONS.
*****DISPLAY "LINK-SUB-CHGDT  = " LINK-SUB-CHGDT   UPON CONS.
*    DISPLAY "LINK-SUB-TANKA  = " LINK-SUB-TANKA   UPON CONS.
*    DISPLAY "LINK-SUB-SURYO  = " LINK-SUB-SURYO   UPON CONS.
*    DISPLAY "LINK-SUB-NOUDT  = " LINK-SUB-NOUDT   UPON CONS.
*****DISPLAY "LINK-SUB-KONPO  = " LINK-SUB-KONPO   UPON CONS.
*****DISPLAY "LINK-SUB-JUURYO = " LINK-SUB-JUURYO  UPON CONS.
*****DISPLAY "LINK-SUB-SSCCCD = " LINK-SUB-SSCCCD  UPON CONS.
*****DISPLAY "LINK-SUB-SURKBN = " LINK-SUB-SURKBN  UPON CONS.
*****DISPLAY "LINK-SUB-KENSURY= " LINK-SUB-KENSURY UPON CONS.
*T↑
*　サブルーチンコール
     CALL     "NJH9440B"       USING   LINK-SUB-IN
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
*　        更新済ＦＬＧ
           MOVE      "1"                     TO   CHG-F23
*　
           REWRITE   CHG-REC
     END-IF.
*
 MAIN-010.
*取込ワーク（発注変更）読込
     PERFORM   YODCHGL1-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             YODCHGL1.
     CLOSE             YODJOHL1.
*
     DISPLAY NC"＃更新起動件数＝" WT-CNT  UPON CONS.
*
 END-EXIT.
     EXIT.
*****************<<  NJH9430B   END PROGRAM  >>******************

```
