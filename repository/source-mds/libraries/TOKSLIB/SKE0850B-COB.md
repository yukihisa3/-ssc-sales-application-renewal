# SKE0850B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SKE0850B.COB`

## ソースコード

```cobol
****************************************************************
*　　顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*　　業務名　　　　　　　：　出荷検品システム                  *
*　　モジュール名　　　　：　送信結果ファイル抽出　　　　　　　*
*　　作成日／更新日　　　：　2001/08/22                        *
*　　作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*　　処理概要　　　　　　：　パラメタより受取った倉庫ＣＤを元に*
*　　　　　　　　　　　　　　送信済キーを抽出する。　　　　　  *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SKE0850B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          01/08/22.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS
         YA        IS   CHR-2
         YB-21     IS   CHR-21
         YB        IS   CHR-15.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 送信検品済キーＦ >>--*
*----<< 検品済キーＦ >>--*
     SELECT   SNDFINF   ASSIGN         DA-01-S-SNDFINF
                        ORGANIZATION   SEQUENTIAL
                        STATUS         FIN-ST.
*----<< 検品済キーＦ >>--*
     SELECT   KENSOKF   ASSIGN         DA-01-VI-KENSOKL2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  KEN-F06
                                       KEN-F01
                                       KEN-F02
                                       KEN-F03
                        STATUS         KEN-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 送信検品済キーＦ >>--*
 FD  SNDFINF            LABEL RECORD   IS   STANDARD
     BLOCK              CONTAINS       81   RECORDS.
     COPY        SNDFINF     OF      XFDLIB
                 JOINING     FIN     PREFIX.
*----<< 検品済キーＦ >>--*
 FD  KENSOKF.
     COPY        KENSOKF     OF      XFDLIB
                 JOINING     KEN     PREFIX.
*
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  9(01).
     03  CHK-FLG        PIC  9(01).
 01  COUNTERS.
     03  IN-CNT         PIC  9(06).
     03  OUT-CNT        PIC  9(06).
*
*----<< ﾜｰｸ ｴﾘｱ >>--*
 01  WK-GOKEI           PIC  9(10)     VALUE  ZERO.
 01  ID                 PIC  9(01)     VALUE  ZERO.
 01  WK-DATE            PIC  9(08).
 01  WK-DATE-R          REDEFINES      WK-DATE.
     03  WK-DATE-R1     PIC  X(04).
     03  WK-DATE-R2     PIC  X(02).
     03  WK-DATE-R3     PIC  X(02).
 01  WK-RCV-F10         PIC  S9(9)V99  VALUE  ZERO.
 01  WK-RCV-F12         PIC  S9(9)V99  VALUE  ZERO.
 01  WK-SURYO           PIC  S9(9)V99  VALUE  ZERO.
 01  P-CNT              PIC  9(03)     VALUE  ZERO.
 01  L-CNT              PIC  9(02)     VALUE  99.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  FIN-ST             PIC  X(02).
 01  KEN-ST             PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-YYMD           PIC  9(08).
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
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-SOKCD                  PIC  X(02).
*
****************************************************************
 PROCEDURE              DIVISION USING       PARA-SOKCD.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 送信検品済キーＦ >>--*
 FIN-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SNDFINF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### SKE0850B SNDFINF    ERROR " FIN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 検品済キーＦ >>--*
 KEN-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      KENSOKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### SKE0850B KENSOKF    ERROR " KEN-ST " "
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
     PERFORM  MAIN-RTN   UNTIL     END-FLG   =    1.
     PERFORM  END-RTN.
     STOP RUN.
 PROG-CNTL-EXIT.
     EXIT.
****************************************************************
*　　　　初期処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-RTN               SECTION.
     ACCEPT   SYS-DATE       FROM DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYS-DATE  TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SKE0850B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     I-O       KENSOKF.
     OPEN     OUTPUT    SNDFINF.
*ワークエリア　クリア
     INITIALIZE         COUNTERS.
     INITIALIZE         FLAGS.
*ファイルスタート
     MOVE     ZERO           TO   KEN-F06.
     MOVE     PARA-SOKCD     TO   KEN-F01.
     MOVE     ZERO           TO   KEN-F02  FIN-F03.
     START    KENSOKF  KEY  IS  >=  KEN-F06 KEN-F01
                                    KEN-F02 KEN-F03
              INVALID
              MOVE     1     TO   END-FLG
              GO             TO   INIT-RTN-EXIT
     END-START.
*
     PERFORM  KENSOKF-READ-SEC.
*
 INIT-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　メイン処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-RTN               SECTION.
*
     MOVE     LINK-IN-YMD8   TO   KEN-F05.
     MOVE     1              TO   KEN-F06.
     REWRITE  KEN-REC.
*データセット
     MOVE     SPACE          TO   FIN-REC.
     INITIALIZE                   FIN-REC.
*
     MOVE     KEN-F04        TO   FIN-F01.
     MOVE     KEN-F02        TO   FIN-F02.
     MOVE     KEN-F03        TO   FIN-F03.
     MOVE     X"0D0A"        TO   FIN-F04.
*
     WRITE    FIN-REC.
*
     PERFORM KENSOKF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　エンド処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*
     CLOSE    SNDFINF   KENSOKF.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SKE0850B END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
 END-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　インプットデータ読む　　　　　　　　　　　　　　　　　*
****************************************************************
 KENSOKF-READ-SEC           SECTION.
*
     READ     KENSOKF  NEXT  AT  END
              MOVE      1    TO   END-FLG
              GO             TO   KENSOKF-READ-EXIT
     END-READ.
*送信ＦＬＧチェック
     IF       KEN-F06  >  ZERO
              MOVE      1    TO   END-FLG
              GO             TO   KENSOKF-READ-EXIT
     END-IF.
*倉庫ＣＤチェック
     IF       KEN-F01  >  PARA-SOKCD
              MOVE      1    TO   END-FLG
     END-IF.
*
 KENSOKF-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
