****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＤＣＭＪＡＰＡＮ　ＷｅｂＥＤＩ　　*
*    モジュール名　　　　：　ケーヨー５０番伝票対応　　　　　　*
*    作成日／更新日　　　：　2018/10/04                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　ケーヨーの５０番伝票の先頭５０を　*
*　　　　　　　　　　　　：　００に変換し更新する。　          *
****************************************************************
*<履歴>*********************************************************
*　変更日　　変更者　　変更変更内容　　　　　　　　　　　　　　*
*  XXXXXXXX  ＮＮＮＮ　ＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮ*
*  20181004  高橋　　　新規作成　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSE8706B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          18/10/04.
 DATE-COMPILED.
 SECURITY.              NONE.
*
 ENVIRONMENT            DIVISION.
*
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         YB        IS   PITCH-1-5
         YB-21     IS   BAIKAKU-1-5
         YA-21     IS   BAIKAKU
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 請求合計ファイル>>-*
     SELECT   SETGK87   ASSIGN         DA-01-VI-SETGK872
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SEI-F01
                                       SEI-F03
                                       SEI-F04
                                       SEI-F05
                        STATUS         SEI-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 請求合計ファイル>>-*
 FD  SETGK87            LABEL RECORD   IS   STANDARD.
     COPY     SETGK87   OF        XFDLIB
              JOINING   SEI       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG           PIC  X(03)    VALUE  SPACE.
 01  COUNTERS.
     03  RD-CNT            PIC  9(06).
     03  HEN-CNT           PIC  9(06).
     03  TAL-CNT           PIC  9(06).
     03  DEN-CNT           PIC  9(06).
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SEI-ST                PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-DATE              PIC  9(06).
 01  FILLER                REDEFINES      SYS-DATE.
     03  SYS-YY            PIC  9(02).
     03  SYS-MM            PIC  9(02).
     03  SYS-DD            PIC  9(02).
 01  SYS-TIME              PIC  9(08).
 01  FILLER                REDEFINES      SYS-TIME.
     03  SYS-HH            PIC  9(02).
     03  SYS-MN            PIC  9(02).
     03  SYS-SS            PIC  9(02).
     03  SYS-MS            PIC  9(02).
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< ワークファイル >>--*
 SETGK87-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SETGK87.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSE8706B SETGK87 ERROR " SEI-ST  " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
****************************************************************
*　　　　メインモジュール　　　　　　　　　　　　　　　　　　　*
****************************************************************
 GENERAL-PROCESS        SECTION.
*
     PERFORM  INIT-RTN.
     PERFORM  MAIN-RTN  UNTIL  END-FLG  =  "END".
     PERFORM  END-RTN.
     STOP RUN.
 GENERAL-EXIT.
     EXIT.
****************************************************************
*　　　　初期処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-RTN               SECTION.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSE8706B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     I-O       SETGK87.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         COUNTERS.
     INITIALIZE         FLAGS.
*
     PERFORM  SEI-READ.
*
 INIT-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　メイン処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-RTN               SECTION.
*    伝票番号先頭”５０”＝＞”００”変換
     MOVE   SEI-F05     TO     SEI-FIL.
     MOVE   "00"        TO     SEI-F05(1:2).
     REWRITE  SEI-REC.
     ADD     1          TO     HEN-CNT.
*
     PERFORM  SEI-READ.
*
 MAIN-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　終了処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*
*    ファイルクローズ
     CLOSE    SETGK87.
*
     DISPLAY "+++ ｾｲｷｭｳﾃﾞｰﾀ INPUT =" RD-CNT  " +++" UPON CONS.
     DISPLAY "+++ 50ﾃﾞﾝﾋﾟｮｳ ﾍﾝｺｳ  =" HEN-CNT " +++" UPON CONS.

     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSE8706B END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS        UPON CONS.
*
 END-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　請求合計ＦＲＥＡＤ　　　　　　　　　　　　　　　　　　*
****************************************************************
 SEI-READ               SECTION.
*
     READ     SETGK87   AT   END
              MOVE     "END"      TO   END-FLG
              GO                  TO   SEI-READ-EXIT
              NOT  AT  END
              ADD       1         TO   RD-CNT
     END-READ.
*
     IF   RD-CNT(3:4) = "0000" OR "5000"
          DISPLAY "RD-CNT = " RD-CNT UPON CONS
     END-IF.
*
     IF    SEI-F01  =  1731 OR 1732 OR 7601 OR 7602
           IF  SEI-F05(1:2)  =  "50"
               CONTINUE
           ELSE
               GO      TO        SEI-READ
           END-IF
     ELSE
           GO          TO        SEI-READ
     END-IF.
*
 SEI-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
