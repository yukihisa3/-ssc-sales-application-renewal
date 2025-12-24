****************************************************************
*　　顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*　　業務名　　　　　　　：　出荷検品システム                  *
*　　モジュール名　　　　：　ケーヨー送信データカウント        *
*　　作成日／更新日　　　：　2010/12/22                        *
*　　作成者／更新者　　　：　NAV                               *
*　　処理概要　　　　　　：　ケーヨー送信データをカウントして、*
*　　　　　　　　　　　　　　件数をパラメタに渡す。　　　　　  *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SKE8020B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          10/12/22.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< ケーヨー送信Ｆ >>--*
     SELECT   SNDKENDT  ASSIGN    TO   DA-01-S-SNDKENDT
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SND-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< ケーヨー送信データ  >>--*
 FD  SNDKENDT           LABEL RECORD   IS   STANDARD
     BLOCK              CONTAINS       31   RECORDS.
 01  SND-REC            PIC  X(128).
*
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  X(03)     VALUE  SPACE.
 01  COUNTERS.
     03  IN-CNT         PIC  9(07)     VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SND-ST             PIC  X(02).
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
 01  PARA-IN-CNT        PIC  9(07).
 01  PARA-KAKUNIN       PIC  X(02).
*
****************************************************************
 PROCEDURE              DIVISION  USING  PARA-IN-CNT
                                         PARA-KAKUNIN.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< ケーヨー送信データ >>--*
 SND-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE   SNDKENDT.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### SKE8020B SNDKENDT ERROR " SND-ST " "
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
     OPEN     INPUT     SNDKENDT.
*ワークエリア　クリア
     INITIALIZE         COUNTERS.
*ケーヨー送信ファイル読込
     PERFORM  SNDKENDT-READ-SEC.
*
 INIT-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　メイン処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-RTN               SECTION.
*ケーヨー送信ファイル読込
     PERFORM  SNDKENDT-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　エンド処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*
     CLOSE    SNDKENDT.
*
     DISPLAY  NC"＃送信件数　＝　" IN-CNT
              NC"　になります。"   UPON CONS.
*読込カウントをパラメタにセット
     MOVE     IN-CNT         TO    PARA-IN-CNT.
*パラ確認に件数情報をセット
     IF       IN-CNT  > ZERO
              MOVE "OK"      TO    PARA-KAKUNIN
     ELSE
              MOVE SPACE     TO    PARA-KAKUNIN
     END-IF.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SKE8020B END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
 END-RTN-EXIT.
     EXIT.
****************************************************************
*　　ケーヨー送信ファイル読込　　　　　　　　　　　　　　　　　*
****************************************************************
 SNDKENDT-READ-SEC       SECTION.
*    ケーヨー送信ファイル
     READ     SNDKENDT  AT  END
              MOVE  "END"    TO   END-FLG
              GO             TO   SNDKENDT-READ-EXIT
     END-READ.
*    読込みカウント
     ADD      1              TO   IN-CNT.
     IF       IN-CNT(4:3) = "000" OR "500"
              DISPLAY "IN-CNT = " IN-CNT UPON CONS
     END-IF.
*
 SNDKENDT-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
