***********************************************************
*
*    顧客名　　　　　：（株）サカタのタネ殿
*    サブシステム　　：ＨＧ基幹システム苗業務連携
*    業務名　　　　　：
*    モジュール名　　：売上伝票ファイル 連携_強制クリア
*    作成日／更新日　：2012/04/18
*    作成者／更新者　：NAV
*    処理概要　　　　：
*      売上伝票ファイルの連携_を強制的にクリアする。
*      非常時の機能にて通常時には必要ない機能。
*    作成日／更新日　：2012/07/12
*    作成者／更新者　：NAV
*    処理概要　　　　：
*      小売連携区分をキーから除外する。
*    作成日／更新日　：2020/05/01
*    作成者／更新者　：NAV T.TAKAHASHI
*    処理概要　　　　：Ｄ３６５連携対応　　先行受注区分初期化
*
***********************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SNACLRNO.
 AUTHOR.               NAV.
 DATE-WRITTEN.         2012/04/18.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE    IS  CONS
     YA         IS  PIT-2
     YA-21      IS  PIT-2-2W
     YA-22      IS  PIT-2-2W2H
     YB         IS  PIT-1V5
     YB-21      IS  PIT-1V5-2W
     YB-22      IS  PIT-1V5-2W2H.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.

*売上伝票ファイル
     SELECT  SHTDENF
       ASSIGN      TO  SHTDENLE
       ORGANIZATION    INDEXED
       ACCESS    MODE  SEQUENTIAL
       RECORD    KEY
*↓2012/07/12
*********URI-F32  *> 小売連携区分
*↑2012/07/12
         URI-F33  *> 連携Ｎｏ
         URI-F112 *> 納品日
       FILE      STATUS    URI-ST.

****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 売上伝票ファイル                                   *
****************************************************************
 FD  SHTDENF
     LABEL     RECORD    IS   STANDARD.
     COPY      SHTDENF   OF   XFDLIB
     JOINING   URI       AS   PREFIX.

****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  URI-ST             PIC  X(02).
*フラグ領域
 01  FLG-AREA.
     03  END-FLG            PIC  X(03)  VALUE  SPACE.
     03  FG-SHTDENF-END     PIC  X(03).
     03  NASI-FLG           PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG            PIC  X(01)  VALUE  SPACE.
*ワーク領域
 01  WRK-AREA.
     03  WK-ACCEPT          PIC  X(01) VALUE SPACE.
     03  CT-IN0             PIC  9(08).
     03  S-NAME-SV          PIC  X(30).
     03  CT-IN              PIC  9(08).
     03  CT-IN2             PIC  9(08).
     03  CT-UP              PIC  9(09).
     03  CT-DL              PIC  9(08).
     03  CT-PG              PIC  9(08).
     03  WK-YMD             PIC  9(08).
     03  WK-YMDR  REDEFINES WK-YMD.
       05  WK-YMD-Y         PIC  9(04).
       05  WK-YMD-M         PIC  9(02).
       05  WK-YMD-D         PIC  9(02).

     03  WK-CMPYMD.
       05  WK-CMPYMD-Y      PIC S9(04).
       05  WK-CMPYMD-M      PIC S9(02).
       05  WK-CMPYMD-D      PIC S9(02).

     03  WK-SYO             PIC  9(06).
     03  WK-AMARI           PIC  9(06).
     03  WK-MATUBI          PIC  9(02).

     03  IX                 PIC  9(04).
     03  IX2                PIC  9(04).

     03  WK-REN-NO-X        PIC  X(09).
     03  WK-REN-NO   REDEFINES WK-REN-NO-X
                            PIC  9(09).
 01  TB-CVT-SU.
     03  TB-CVT-SU          PIC  X(11)  VALUE
         "0123456789 ".
     03  TB-CVT-SUR  REDEFINES TB-CVT-SU
                            PIC  X(01)  OCCURS 11.

     03  TB-CVT-SU-N        PIC  N(11)  VALUE
       NC"０１２３４５６７８９　".
     03  TB-CVT-SU-NR  REDEFINES TB-CVT-SU-N
                            PIC  N(01)  OCCURS 11.

 01  WK-RENNO-G.
     03  WK-RENNO           PIC  X(09).
     03  WK-RENNOR-9  REDEFINES  WK-RENNO
                            PIC  9(09).
     03  WK-RENNOR    REDEFINES  WK-RENNO
                            PIC  X(01)  OCCURS 9.

     03  WK-RENNO-N         PIC  N(09).
     03  WK-RENNO-NR  REDEFINES  WK-RENNO-N
                            PIC  N(01)  OCCURS 9.

 01  WK-DTKENSU-G.
     03  WK-DTKENSU         PIC  X(06).
     03  WK-DTKENSUR-9  REDEFINES  WK-DTKENSU
                            PIC  9(06).
     03  WK-DTKENSUR-Z  REDEFINES  WK-DTKENSU
                            PIC  ZZZZZ9.
     03  WK-DTKENSUR    REDEFINES  WK-DTKENSU
                            PIC  X(01)  OCCURS 6.

     03  WK-DTKENSU-N       PIC  N(06).
     03  WK-DTKENSU-NR  REDEFINES  WK-DTKENSU-N
                            PIC  N(01)  OCCURS 6.

*  エラーセクション名
 01  SEC-NAME.
     03  FILLER             PIC  X(05)  VALUE " *** ".
     03  S-NAME             PIC  X(30).

*　システム日付／時刻
 01  TIME-AREA.
     03  WK-TIME            PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-DATE            PIC  9(06)  VALUE  ZERO.
     03  SYS-DATE           PIC  9(08)  VALUE  ZERO.

*　日付論理チェック
 01  WK-CHKDATE.
     03  WK-CHKDATE-YYYY    PIC  9(04)  VALUE  ZERO.
     03  WK-CHKDATE-MM      PIC  9(02)  VALUE  ZERO.
     03  WK-CHKDATE-DD      PIC  9(02)  VALUE  ZERO.
*
 01  FILE-ERR.
     03  URI-ERR           PIC  N(20)  VALUE
         NC"売上伝票ファイルエラー".
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
****************************************************************
 LINKAGE               SECTION.
****************************************************************
* 入力パラメータ
 01  RENKEI-NO             PIC  X(09).
* 出力パラメータ
*
**************************************************************
 PROCEDURE             DIVISION  USING RENKEI-NO.
**************************************************************
 DECLARATIVES.
 URI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SHTDENF.
     DISPLAY     URI-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     URI-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE  "PROCESS START"       TO  S-NAME.

     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC  UNTIL END-FLG = "END".
     PERFORM  END-SEC.

     STOP RUN.
 CONTROL-EXIT.
     EXIT.
****************************************************************
*             初期処理                               1.0
****************************************************************
 INIT-SEC              SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "INIT-SEC"       TO  S-NAME.

     DISPLAY   "**  START SNACLRNO  **"  UPON CONS.

     DISPLAY NC"指定した連携ＮＯ＝" RENKEI-NO UPON CONS.
 INIT-00.
     DISPLAY NC"本当に連携ＮＯをクリアしますか？（Ｙ／Ｎ）"
                                                     UPON CONS.
     ACCEPT  WK-ACCEPT  FROM  CONS.

     IF      WK-ACCEPT  = "Y"
             GO       TO     INIT-01
     END-IF.
     IF      WK-ACCEPT  = "N"
             DISPLAY NC"＃＃　中　止　＃＃" UPON CONS
             MOVE    "END"   TO   END-FLG
             STOP     RUN
       ELSE
             GO       TO     INIT-00
     END-IF.
*
 INIT-01.
*
     PERFORM  SDATE-GET-SEC.
*ファイルのＯＰＥＮ
     OPEN  I-O    SHTDENF.
*ワークの初期化
     INITIALIZE  WRK-AREA.
     INITIALIZE  FLG-AREA.
*
     MOVE  S-NAME-SV        TO  S-NAME.
 INIT-EXIT.
     EXIT.
****************************************************************
*    システム日付取得                                          *
****************************************************************
 SDATE-GET-SEC              SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "SDATE-GET-SEC"  TO  S-NAME.
*システム日付・時刻の取得
     ACCEPT  WK-DATE   FROM DATE.
     MOVE  "3"              TO  LINK-IN-KBN.
     MOVE  WK-DATE          TO  LINK-IN-YMD6.
     MOVE  ZERO             TO  LINK-IN-YMD8.
     MOVE  ZERO             TO  LINK-OUT-RET.
     MOVE  ZERO             TO  LINK-OUT-YMD.
     CALL  "SKYDTCKB"  USING LINK-IN-KBN
                             LINK-IN-YMD6
                             LINK-IN-YMD8
                             LINK-OUT-RET
                             LINK-OUT-YMD.
     MOVE  LINK-OUT-YMD     TO  SYS-DATE.
     ACCEPT  WK-TIME   FROM TIME.

     MOVE  S-NAME-SV        TO  S-NAME.
 SDATE-GET-EXIT.
     EXIT.
***************************************************************
*             メイン処理                             2.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "MAIN-SEC"       TO  S-NAME.

     PERFORM  UPD-SHTDENF-SEC.

     MOVE  S-NAME-SV        TO  S-NAME.
 MAIN-EXIT.
     EXIT.
****************************************************************
*  売上伝票ファイル更新                                        *
****************************************************************
 UPD-SHTDENF-SEC            SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "UPD-SHTDENF-SEC"  TO  S-NAME.

     INITIALIZE  URI-REC.
*↓2012/07/12
*****MOVE  1                TO  URI-F32.  *> 小売連携区分
*↑2012/07/12
     MOVE  RENKEI-NO        TO  URI-F33.  *> 連携Ｎｏ
     MOVE  LOW-VALUE        TO  FG-SHTDENF-END.

     PERFORM  RD-SHTDENF-SEC.  *> 売上伝票の初期読込み

     PERFORM  UNTIL FG-SHTDENF-END = "END"
       PERFORM  UPD-SHTDENFB-SEC
       PERFORM  RD-SHTDENF-SEC

     END-PERFORM.
     MOVE  "END"            TO  END-FLG.

     MOVE  S-NAME-SV        TO  S-NAME.
 UPD-SHTDENF-EXIT.
     EXIT.
****************************************************************
*    売上伝票ファイル初期読込み                           *
****************************************************************
 RD-SHTDENF-SEC          SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "ST-SHTDENF-SEC" TO  S-NAME.

     IF  FG-SHTDENF-END = LOW-VALUE
         START  SHTDENF  KEY >=
*↓2012/07/12
****************URI-F32  *> 小売連携区分
*↑2012/07/12
                URI-F33  *> 連携Ｎｏ
                URI-F112 *> 納品日
           INVALID KEY
              DISPLAY NC"！！！！！！！！！！！！！！" UPON CONS
              DISPLAY NC"！　指定された連携ＮＯは　！" UPON CONS
              DISPLAY NC"！　売上伝票ファイルに　　！" UPON CONS
              DISPLAY NC"！　存在しません！？　　　！" UPON CONS
              DISPLAY NC"！！！！！！！！！！！！！！" UPON CONS
              MOVE  "END"   TO  FG-SHTDENF-END
              GO TO  RD-SHTDENF-090
         END-START

         MOVE  SPACE        TO  FG-SHTDENF-END

     END-IF.

     READ  SHTDENF
       AT  END
         MOVE  "END"        TO  FG-SHTDENF-END
         GO TO  RD-SHTDENF-090
     END-READ.
*↓2012/07/12
*****IF  URI-F32 = "1" *> 小売連携区分あり
*****    CONTINUE
*****ELSE
*****    MOVE  "END"        TO  FG-SHTDENF-END
*****    GO TO  RD-SHTDENF-090
*****END-IF.
*↑2012/07/12
     IF  URI-F33 = RENKEI-NO  *> 連携Ｎｏ
         CONTINUE
     ELSE
         MOVE  "END"        TO  FG-SHTDENF-END
         GO TO  RD-SHTDENF-090
     END-IF.

     ADD 1   TO  CT-IN.

 RD-SHTDENF-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 RD-SHTDENF-EXIT.
     EXIT.
****************************************************************
*    売上伝票ファイル更新Ｂ                                    *
****************************************************************
 UPD-SHTDENFB-SEC      SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "UPD-SHTDENFB-SEC" TO  S-NAME.

     MOVE  SPACE            TO  URI-F33. *> 小売連携Ｎｏ
*# 2020/05/01 NAV ST Ｄ３６５連携対応
     MOVE  SPACE            TO  URI-D95. *> 先行受注連携区分
*# 2020/05/01 NAV ED Ｄ３６５連携対応
     REWRITE  URI-REC.
     ADD  1   TO  CT-UP.

     MOVE  S-NAME-SV        TO  S-NAME.
 UPD-SHTDENFB-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "END-SEC"        TO  S-NAME.
*
*ファイル ＣＬＯＳＥ
     DISPLAY NC"＊＊＊＊＊＊＊＊＊＊＊＊＊＊" UPON CONS.
     DISPLAY NC"指定した連携ＮＯ＝" RENKEI-NO UPON CONS.
     DISPLAY NC"クリアした件数　＝" CT-UP     UPON CONS.
     DISPLAY NC"＊＊＊＊＊＊＊＊＊＊＊＊＊＊" UPON CONS.
     CLOSE  SHTDENF.
     DISPLAY   "**  END   SNACLRNO  **"  UPON CONS.
*
     MOVE  S-NAME-SV        TO  S-NAME.
*
 END-EXIT.
     EXIT.
*****************<<  SNACLRNO   END PROGRAM  >>******************
