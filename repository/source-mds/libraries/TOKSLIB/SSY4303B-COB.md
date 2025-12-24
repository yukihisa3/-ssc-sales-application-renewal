# SSY4303B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SSY4303B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　アークランドサカモトオンライン　　*
*    業務名　　　　　　　：　欠品案内データ抽出　　　　        *
*    モジュール名　　　　：　欠品案内データ抽出                *
*    作成日／更新日　　　：　02/01/08                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*    更新日／更新者　　　：　11/10/07 /YOSHIDA.M               *
*    更新概要　　　　　　：　基幹サーバ統合　　　　　　　　　　*
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSY4303B.
 AUTHOR.               NAV.
 DATE-WRITTEN.         02/01/08.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     YA           IS   CHR-2
     YB-21        IS   CHR-21
     YB           IS   CHR-15
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*売上伝票ファイル
     SELECT  SHTDENF   ASSIGN    TO        SHTDENLA
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       DEN-F46
                                           DEN-F47
                                           DEN-F01
                                           DEN-F48
                                           DEN-F02
                                           DEN-F04
***2011.10.07 ST
***                                        DEN-F132
                                           DEN-F051
                                           DEN-F07
                                           DEN-F112
***2011.10.07 EN
                                           DEN-F03
                       FILE      STATUS    DEN-ST.
*欠品案内ファイル
     SELECT  ARKKEPF   ASSIGN    TO        ARKKEPL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       ARK-F01
                                           ARK-F02
                                           ARK-F03
                                           ARK-F04
                                           ARK-F05
                       FILE      STATUS    ARK-ST.
*欠品案内エラーファイル
     SELECT  ARKERRF   ASSIGN    TO        ARKERRF
                       ACCESS    MODE      SEQUENTIAL
                       FILE      STATUS    ARE-ST.
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 売上伝票ファイル                     *
****************************************************************
 FD  SHTDENF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      SHTDENF   OF   XFDLIB
                       JOINING   DEN       AS   PREFIX.
****************************************************************
*    FILE = 欠品案内ファイル                                   *
****************************************************************
 FD  ARKKEPF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      ARKKEPF   OF   XFDLIB
                       JOINING   ARK       AS   PREFIX.
****************************************************************
*  FILE= 欠品案内エラー                                      *
****************************************************************
 FD  ARKERRF
                       BLOCK     CONTAINS  40   RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      ARKERRF   OF   XFDLIB
                       JOINING   ARE       AS   PREFIX.
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  DEN-ST                   PIC  X(02).
     03  ARK-ST                   PIC  X(02).
     03  ARE-ST                   PIC  X(02).
*フラグ領域
 01  FLG-AREA.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  ARK-INV-FLG              PIC  X(03)  VALUE  SPACE.
*ワーク領域
 01  WRK-AREA.
     03  READ-CNT                 PIC  9(07)  VALUE  ZERO.
     03  ERR-CNT                  PIC  9(07)  VALUE  ZERO.
     03  WRITE-CNT                PIC  9(07)  VALUE  ZERO.
     03  REWRITE-CNT              PIC  9(07)  VALUE  ZERO.
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
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
 01  FILE-ERR.
     03  DEN-ERR           PIC N(20) VALUE
                        NC"売上伝票ファイルエラー".
     03  ARK-ERR           PIC N(20) VALUE
                        NC"欠品案内ファイルエラー".
     03  ARE-ERR           PIC N(20) VALUE
                        NC"欠品案内エラーＦエラー".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
**************************************************************
 LINKAGE                   SECTION.
 01  PARA-BTDATE                 PIC  9(08).
 01  PARA-BTTIME                 PIC  9(04).
 01  PARA-BTTORI                 PIC  9(08).
**************************************************************
 PROCEDURE             DIVISION  USING  PARA-BTDATE
                                        PARA-BTTIME
                                        PARA-BTTORI.
**************************************************************
 DECLARATIVES.
 DEN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SHTDENF.
     DISPLAY     DEN-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     DEN-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 ARK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ARKKEPF.
     DISPLAY     ARK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ARK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 ARE-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ARKERRF.
     DISPLAY     ARE-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ARE-ST    UPON      CONS.
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
 PROCESS-EXIT.
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
*ファイルのＯＰＥＮ
     OPEN     I-O       ARKKEPF   SHTDENF.
     OPEN     OUTPUT    ARKERRF.
*ワークの初期化
     INITIALIZE         FLG-AREA.
*売上伝票ファイルスタート
     PERFORM  DEN-START-SEC.
*判定
     IF   END-FLG  NOT =  "END"
*         売上伝票ファイル読込み
          PERFORM  DEN-READ-SEC
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             2.0
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*データ存在チェック
     MOVE      DEN-F46       TO   ARK-F01.
     MOVE      DEN-F47       TO   ARK-F02.
     MOVE      DEN-F01       TO   ARK-F03.
     MOVE      DEN-F02       TO   ARK-F04.
     MOVE      DEN-F03       TO   ARK-F05.
     PERFORM   ARK-READ-SEC.
     IF        ARK-INV-FLG  =  SPACE
               IF  ARK-F07  =  "1"
                   PERFORM ARKERRF-WRITE-SEC
               ELSE
                   PERFORM ARKKEPF-REWRITE-SEC
               END-IF
     ELSE
               PERFORM ARKKEPF-WRITE-SEC
     END-IF.
*売上伝票ファイル読込み
     PERFORM   DEN-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               6.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             SHTDENF  ARKKEPF  ARKERRF.
*
     DISPLAY "SHTDENF  READ  CNT   = "  READ-CNT     UPON CONS.
     DISPLAY "ARKERRF  WRITE CNT   = "  ERR-CNT      UPON CONS.
     DISPLAY "ARKKEPF  WRITE CNT   = "  WRITE-CNT    UPON CONS.
     DISPLAY "ARKKEPF  REWRITE CNT = "  REWRITE-CNT  UPON CONS.
*
 END-EXIT.
     EXIT.
****************************************************************
*             売上伝票ファイルＳＴＡＲＴ                       *
****************************************************************
 DEN-START-SEC         SECTION.
     MOVE     "DEN-START-SEC"     TO   S-NAME.
*
***2011.10.07 ST
     MOVE     SPACE          TO   DEN-REC.
     INITIALIZE                   DEN-REC.
***2011.10.07 EN
     MOVE     PARA-BTDATE    TO   DEN-F46.
     MOVE     PARA-BTTIME    TO   DEN-F47.
     MOVE     PARA-BTTORI    TO   DEN-F01.
     MOVE     SPACE          TO   DEN-F48.
***2011.10.07 ST
***  START    SHTDENF   KEY  IS   >=   DEN-F46
***                                    DEN-F47
***                                    DEN-F01
***                                    DEN-F48
     START    SHTDENF   KEY  IS   >=   DEN-F46
                                       DEN-F47
                                       DEN-F01
                                       DEN-F48
                                       DEN-F02
                                       DEN-F04
                                       DEN-F051
                                       DEN-F07
                                       DEN-F112
                                       DEN-F03
***2011.10.07 EN
        INVALID
              MOVE         "END"  TO   END-FLG
     END-START.
*
 DEN-START-EXIT.
     EXIT.
****************************************************************
*             売上伝票ファイルＲＥＡＤ　                       *
****************************************************************
 DEN-READ-SEC          SECTION.
     MOVE     "DEN-READ-SEC"      TO   S-NAME.
*
     READ     SHTDENF       AT    END
              MOVE         "END"  TO   END-FLG
              GO   TO   DEN-READ-EXIT
     END-READ.
*
     ADD      1         TO        READ-CNT.
*終了判定
     IF       DEN-F46   NOT =     PARA-BTDATE
          OR  DEN-F47   NOT =     PARA-BTTIME
          OR  DEN-F01   NOT =     PARA-BTTORI
              MOVE     "END"      TO   END-FLG
              GO   TO   DEN-READ-EXIT
     END-IF.
*訂正数量と発注数量が同じ物は読み飛ばし
     IF       DEN-F50   =   DEN-F15
              GO   TO   DEN-READ-SEC
     END-IF.
*
 DEN-READ-EXIT.
     EXIT.
****************************************************************
*    欠品案内エラーファイル作成                      2.1       *
****************************************************************
 ARKERRF-WRITE-SEC    SECTION.
     MOVE     "ARKERRF-WRITE-SEC"    TO   S-NAME.
*
     MOVE      SPACE      TO     ARE-REC.
     INITIALIZE                  ARE-REC.
*
     MOVE      DEN-F46    TO     ARE-F01.
     MOVE      DEN-F47    TO     ARE-F02.
     MOVE      DEN-F01    TO     ARE-F03.
     MOVE      DEN-F02    TO     ARE-F04.
     MOVE      DEN-F03    TO     ARE-F05.
     MOVE      ARK-F06    TO     ARE-F06.
     MOVE      "1"        TO     ARE-F07.
     MOVE      ARK-F08    TO     ARE-F08.
*    レコード追加
     WRITE     ARE-REC.
*    件数カウント
     ADD       1          TO     ERR-CNT.
*
 ARKERRF-WRITE-EXIT.
     EXIT.
****************************************************************
*    欠品案内ファイル作成                            2.2       *
****************************************************************
 ARKKEPF-WRITE-SEC        SECTION.
     MOVE     "ARKKEPF-WRITE-SEC"    TO   S-NAME.
*
     MOVE      SPACE      TO     ARK-REC.
     INITIALIZE                  ARK-REC.
*
     MOVE      DEN-F46    TO     ARK-F01.
     MOVE      DEN-F47    TO     ARK-F02.
     MOVE      DEN-F01    TO     ARK-F03.
     MOVE      DEN-F02    TO     ARK-F04.
     MOVE      DEN-F03    TO     ARK-F05.
     MOVE      DEN-F15    TO     ARK-F06.
*    レコード追加
     WRITE     ARK-REC.
*    件数カウント
     ADD       1          TO     WRITE-CNT.
*
 ARKKEPF-WRITE-EXIT.
     EXIT.
****************************************************************
*    欠品案内ファイル更新                            2.3       *
****************************************************************
 ARKKEPF-REWRITE-SEC        SECTION.
     MOVE     "ARKKEPF-REWRITE-SEC"    TO   S-NAME.
*
     MOVE      DEN-F15    TO     ARK-F06.
*    レコード追加
     REWRITE   ARK-REC.
*    件数カウント
     ADD       1          TO     REWRITE-CNT.
*
 ARKKEPF-REWRITE-EXIT.
     EXIT.
****************************************************************
*    欠品案内ファイル読込み                          2.4       *
****************************************************************
 ARK-READ-SEC               SECTION.
     MOVE     "ARK-READ-SEC"  TO   S-NAME.
*
     READ  ARKKEPF
           INVALID      MOVE   "INV"   TO   ARK-INV-FLG
           NOT INVALID  MOVE   SPACE   TO   ARK-INV-FLG
     END-READ.
*
 ARK-READ-EXIT.
     EXIT.
*****************<<  SSY4303B   END PROGRAM  >>******************

```
