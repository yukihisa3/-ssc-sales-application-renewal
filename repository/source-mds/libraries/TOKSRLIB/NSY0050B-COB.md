# NSY0050B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NSY0050B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＤＣＭ仕入先統合　　　　　　　　　*
*    業務名　　　　　　　：　発注業務　　　　                  *
*    モジュール名　　　　：　発注受信データ変換連続処理　　　  *
*    作成日／更新日　　　：　21/02/15                          *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　新ＤＣＭ発注件数ファイルを指定され*
*                            たバッチＮＯでスタートし、変換処理*
*                            を起動する。                      *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           NSY0050B.
 AUTHOR.               NAV.
 DATE-WRITTEN.         21/02/15.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     YA           IS   YA
     YB           IS   YB
     YA-22        IS   YA-22
     YB-21        IS   YB-21
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*新ＤＣＭ発注件数ファイル
     SELECT  DCMKENF      ASSIGN    TO     DA-01-VI-DCMKENL1
                          ORGANIZATION     INDEXED
                          ACCESS    MODE   SEQUENTIAL
                          RECORD    KEY    KEN-F01
                                           KEN-F02
                                           KEN-F03
                          FILE      STATUS KEN-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = イオン取引先受信ワーク                             *
****************************************************************
 FD  DCMKENF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      DCMKENF   OF   XFDLIB
                       JOINING   KEN       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  KEN-ST                   PIC  X(02).
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  KEN-ERR           PIC N(15) VALUE
         NC"新ＤＣＭ発注件数ファイルエラー".
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
***  ﾜｰｸｴﾘｱ
 01  WK-AREA.
     03  END-FLG           PIC X(03) VALUE     SPACE.
     03  P-CNT             PIC 9(05) VALUE     ZERO.
     03  L-CNT             PIC 9(05) VALUE     ZERO.
     03  SYSDATE           PIC 9(06) VALUE     ZERO.
     03  IX                PIC 9(02) VALUE     ZERO.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
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
*開始時刻／終了時刻保管
 01  WK-JOB-TIME.
     03  WK-ST-TIME               PIC  9(06)  VALUE  ZERO.
     03  WK-ED-TIME               PIC  9(06)  VALUE  ZERO.
*
 01  WK-KEKA                      PIC  X(01)  VALUE  SPACE.
*------------------------------------------------------------*
 LINKAGE              SECTION.
*------------------------------------------------------------*
*パラメタ取得
 01  LINK-JDATE            PIC 9(08).
 01  LINK-JTIME            PIC 9(04).
*
**************************************************************
 PROCEDURE             DIVISION   USING    LINK-JDATE
                                           LINK-JTIME.
**************************************************************
 DECLARATIVES.
 KEN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DCMKENF.
     MOVE        KEN-ST    TO        E-ST.
     MOVE       "DCMKENL1" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     KEN-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     DISPLAY NC"＃新ＤＣＭ発注件数ファイル異常＃" UPON CONS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
     DISPLAY NC"＃新ＤＣＭ発注連続変換　　開始＃" UPON CONS.
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC  UNTIL  END-FLG = "END".
     PERFORM   END-SEC.
     DISPLAY NC"＃新ＤＣＭ発注連続変換　　終了＃" UPON CONS.
     STOP  RUN.
 PROCESS-END.
     EXIT.
****************************************************************
*             初期処理                               0.0       *
****************************************************************
 INIT-SEC              SECTION.
     MOVE     "INIT-SEC"     TO   S-NAME.
*システム日付・時刻の取得
     ACCEPT  WK-DATE  FROM DATE.
     MOVE  "3"                   TO  LINK-IN-KBN.
     MOVE  WK-DATE               TO  LINK-IN-YMD6.
     MOVE  ZERO                  TO  LINK-IN-YMD8.
     MOVE  ZERO                  TO  LINK-OUT-RET.
     MOVE  ZERO                  TO  LINK-OUT-YMD.
     CALL  "SKYDTCKB"  USING LINK-IN-KBN
                             LINK-IN-YMD6
                             LINK-IN-YMD8
                             LINK-OUT-RET
                             LINK-OUT-YMD.
     MOVE  LINK-OUT-YMD          TO  DATE-AREA.
*ファイルのＯＰＥＮ
     OPEN      I-O    DCMKENF.
*イオン取引先受信ワークスタート
     MOVE     SPACE       TO      KEN-REC.
     INITIALIZE                   KEN-REC.
     MOVE     LINK-JDATE  TO      KEN-F01.
     MOVE     LINK-JTIME  TO      KEN-F02.
     START  DCMKENF  KEY  IS >=   KEN-F01  KEN-F02  KEN-F03
            INVALID
            DISPLAY NC"＃＃指定バッチＮＯ無１＃＃" UPON CONS
            MOVE  "END"   TO      END-FLG
            GO            TO      INIT-EXIT
     END-START.
*
     PERFORM  DCMKENF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             1.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*変換開始時刻取得　　
     ACCEPT  WK-TIME  FROM TIME.
     MOVE    WK-TIME(1:6)    TO   WK-ST-TIME.
*    変換ＰＧ起動
     DISPLAY "KEN => " KEN-F01 " - " KEN-F02 " - " KEN-F03
     UPON CONS.
     CALL     "PNSY0040"     USING   KEN-F01
                                     KEN-F02
                                     KEN-F03.
*                                    WK-KEKA.
*変換終了時刻取得　　
     ACCEPT  WK-TIME  FROM TIME.
     MOVE    WK-TIME(1:6)    TO   WK-ED-TIME.
*開始時刻、終了時刻更新、結果更新
     MOVE    WK-ST-TIME      TO   KEN-F05.
     MOVE    WK-ED-TIME      TO   KEN-F06.
     MOVE    WK-KEKA         TO   KEN-F07.
     REWRITE KEN-REC.
*次レコード読み込み
     PERFORM  DCMKENF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*    イオン取引先受信ワーク読込                      ALL       *
****************************************************************
 DCMKENF-READ-SEC      SECTION.
     MOVE "DCMKENF-READ-SEC" TO   S-NAME.
*
     READ  DCMKENF  AT  END
           MOVE   "END"      TO   END-FLG
           GO                TO   DCMKENF-READ-EXIT
     END-READ.
     DISPLAY "KEN-F01 = " KEN-F01 UPON  CONS.
     DISPLAY "KEN-F02 = " KEN-F02 UPON  CONS.
*バッチＮＯ確認　　　　　　　　　　　　　　　　　　　　
     IF    KEN-F01  =  LINK-JDATE
     AND   KEN-F02  =  LINK-JTIME
           CONTINUE
     ELSE
           MOVE   "END"      TO   END-FLG
           GO                TO   DCMKENF-READ-EXIT
     END-IF.
*
 DCMKENF-READ-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE     "END-SEC"      TO   S-NAME.
*ファイルのＯＰＥＮ
     CLOSE     DCMKENF.
*
 END-EXIT.
     EXIT.
*****************<<  NSY0050B   END PROGRAM  >>******************

```
