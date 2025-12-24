# NJH1203L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/NJH1203L.COB`

## ソースコード

```cobol
****************************************************************
*　　顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*　　業務名　　　　　　　：　コーナンオンラインシステム　      *
*　　モジュール名　　　　：　コーナンオンラインデータチェック  *
*　　作成日／更新日　　　：　2010/12/23                        *
*　　作成者／更新者　　　：　NAV                               *
*　　処理概要　　　　　　：　コーナン受信データを読み、レコード*
*　　　　　　　　　　　　　　区分を判断して、件数をカウントする*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NJH1203L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          10/12/23.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< ケーヨー送信Ｆ >>--*
     SELECT   ONLKONAN  ASSIGN    TO   DA-01-S-ONLKONAN
                        ORGANIZATION   SEQUENTIAL
                        STATUS         KON-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< ケーヨー送信データ  >>--*
 FD  ONLKONAN           LABEL RECORD   IS   STANDARD
     BLOCK              CONTAINS       1    RECORDS.
 01  KON-REC.
   03  KONAN-F01.
       05  KONAN-F011   PIC  X(02).
       05  FILLER       PIC  X(126).
   03  KONAN-F02.
       05  KONAN-F021   PIC  X(02).
       05  FILLER       PIC  X(126).
*
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  X(03)     VALUE  SPACE.
 01  COUNTERS.
     03  IN-CNT         PIC  9(07)     VALUE  ZERO.
     03  DD-CNT         PIC  9(07)     VALUE  ZERO.
     03  DJ-CNT         PIC  9(07)     VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  KON-ST             PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  WK-SYS-DATE        PIC  9(08).
 01  WK-SYS-TIME        PIC  9(06).
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
 01  LINK-AREA2.
     03  LINK-IN-KBN    PIC  X(01).
     03  LINK-IN-YMD6   PIC  9(06).
     03  LINK-IN-YMD8   PIC  9(08).
     03  LINK-OUT-RET   PIC  X(01).
     03  LINK-OUT-YMD8  PIC  9(08).
 LINKAGE                SECTION.
 01  PARA-DD-CNT        PIC  9(07).
 01  PARA-DJ-CNT        PIC  9(07).
*
****************************************************************
 PROCEDURE              DIVISION  USING  PARA-DD-CNT
                                         PARA-DJ-CNT.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< ケーヨー送信データ >>--*
 KON-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE   ONLKONAN.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### NJH1203L ONLKONAN ERROR " KON-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
****************************************************************
*　　　　メインモジュール　　　　　　　　　　　　　　　　　　　*
****************************************************************
 PROG-CNTL              SECTION.
     PERFORM  INIT-RTN.
     PERFORM  MAIN-RTN   UNTIL     END-FLG   =   "END".
     PERFORM  END-RTN.
     STOP RUN.
 PROG-CNTL-EXIT.
     EXIT.
****************************************************************
*　　　　初期処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-RTN               SECTION.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
*    システム日付８桁変換
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYS-DATE  TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     MOVE     LINK-OUT-YMD8  TO   WK-SYS-DATE.
     MOVE     SYS-TIME(1:6)  TO   WK-SYS-TIME.
*ファイルＯＰＥＮ
     OPEN     INPUT     ONLKONAN.
*ワークエリア　クリア
     INITIALIZE         COUNTERS.
*コーナン受信Ｆ読込
     PERFORM  ONLKONAN-READ-SEC.
*
 INIT-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　メイン処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-RTN               SECTION.
*発注明細／受領明細数をカウントする。
     IF  KONAN-F011  NOT =  SPACE
         EVALUATE  KONAN-F011
             WHEN  "DD"  ADD    1     TO    DD-CNT
             WHEN  "DJ"  ADD    1     TO    DJ-CNT
         END-EVALUATE
     END-IF.
*
     IF  KONAN-F021  NOT =  SPACE
         EVALUATE  KONAN-F021
             WHEN  "DD"  ADD    1     TO    DD-CNT
             WHEN  "DJ"  ADD    1     TO    DJ-CNT
         END-EVALUATE
     END-IF.
*コーナン受信Ｆ読込
     PERFORM  ONLKONAN-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　エンド処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*
     CLOSE    ONLKONAN.
*読込カウントをパラメタにセット
     MOVE     DD-CNT         TO    PARA-DD-CNT.
     MOVE     DJ-CNT         TO    PARA-DJ-CNT.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** NJH1203L END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
 END-RTN-EXIT.
     EXIT.
****************************************************************
*　　コーナン受信Ｆ読込　　　　　　　　　　　　　　　　　*
****************************************************************
 ONLKONAN-READ-SEC       SECTION.
*    コーナン受信Ｆ
     READ     ONLKONAN  AT  END
              MOVE  "END"    TO   END-FLG
              GO             TO   ONLKONAN-READ-EXIT
     END-READ.
*    読込みカウント
     ADD      1              TO   IN-CNT.
     IF       IN-CNT(5:3) = "000" OR "500"
              DISPLAY "IN-CNT = " IN-CNT UPON CONS
     END-IF.
*
 ONLKONAN-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
