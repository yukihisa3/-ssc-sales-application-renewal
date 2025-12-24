****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＤＣＭＪＡＰＡＮ　ＷｅｂＥＤＩ　　*
*    モジュール名　　　　：　請求データ伝票番号リカバリ
*    作成日／更新日　　　：　2021/01/26                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　請求合計ファイルより読み、伝票番号*
*　　　　　　　　　　　　：　の再取得を行ない、別ファイルへ出力*
*                        ：　する。　　　　　　　　　　　　　　*
*<履歴>*********************************************************
*　変更日　　変更者　　変更変更内容　　　　　　　　　　　　　　*
*  XXXXXXXX  ＮＮＮＮ　ＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮ*
*  20210126  高橋　　　新規作成（ＳＳＥ８７１０Ｂ流用）　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSE8797B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2011/08/02.
 DATE-COMPILED.
 SECURITY.              NONE.
*
 ENVIRONMENT            DIVISION.
*
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 請求合計ファイル >>-*
     SELECT   SETGK87   ASSIGN         DA-01-VI-SETGK871
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SEI-F01
                                       SEI-F05
                                       WITH DUPLICATES
                        STATUS         SEI-ST.
*----<< ＤＣＭ出荷情報ファイル >>--*
     SELECT   DJSYUKF   ASSIGN         DA-01-VI-DJSYUKL9
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  DJS-K03
                                       DJS-K05
                                       DJS-K08
                                       DJS-K06
                                       DJS-K07
                        STATUS         DJS-ST.
*----<< ＤＣＭ請求合計ファイル（資材）>>--*
     SELECT   SETGK87W  ASSIGN         DA-01-VS-SETGK87W
                        STATUS         SG7-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 請求合計ファイル >>-*
 FD  SETGK87            LABEL RECORD   IS   STANDARD.
     COPY     SETGK87   OF        XFDLIB
              JOINING   SEI       PREFIX.
*----<< ＤＣＭ出荷情報ファイル >>-*
 FD  DJSYUKF            LABEL RECORD   IS   STANDARD.
     COPY     DJSYUKF   OF        XFDLIB
              JOINING   DJS       PREFIX.
*----<< ＤＣＭ請求合計ファイル（資材）>>-*
 FD  SETGK87W           LABEL RECORD   IS   STANDARD.
     COPY     SETGK87W  OF        XFDLIB
              JOINING   SG7       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG           PIC  X(03)    VALUE  SPACE.
     03  DJS-INV-FLG       PIC  X(03)    VALUE  SPACE.
     03  SG87-HIT-FLG      PIC  X(01)    VALUE  SPACE.
 01  COUNTERS.
     03  RD-CNT            PIC  9(07).
     03  IN01-CNT          PIC  9(07).
     03  SG873-CNT         PIC  9(07).
     03  SG883-CNT         PIC  9(07).
     03  SG7-READ-CNT      PIC  9(07).
     03  SG8-READ-CNT      PIC  9(07).
     03  SG7-ERR           PIC  9(07).
     03  SG8-ERR           PIC  9(07).
*
 01  IX1                   PIC  9(02).
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SEI-ST                PIC  X(02).
 01  DJS-ST                PIC  X(02).
 01  SG7-ST                PIC  X(02).
*----<< ﾜｰｸ ｴﾘｱ >>--*
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
*末日取得サブルーチン
 01  SSKTLSTD-DATE      PIC  9(08).
 01  FILLER             REDEFINES      SSKTLSTD-DATE.
     03  SSKTLSTD-YY    PIC  9(04).
     03  SSKTLSTD-MM    PIC  9(02).
     03  SSKTLSTD-DD    PIC  9(02).
 01  SSKTLSTD-RET       PIC  9(01).
*
 01  SIME-DATE          PIC  9(08).
 01  FILLER             REDEFINES      SIME-DATE.
     03  SIME-YY        PIC  9(04).
     03  SIME-MM        PIC  9(02).
     03  SIME-DD        PIC  9(02).
*
 01  LINK-AREA2.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 請求合計ファイル >>--*
 SETGK87-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SETGK87.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSE8797B SETGK87 ERROR " SEI-ST  " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< ＤＣＭ出荷情報ファイル >>--*
 DJSYUKF-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DJSYUKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSE8797B DJSYUKF ERROR " DJS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< ＤＣＭ請求合計ファイル（資材）>>--*
 SETGK87-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SETGK87W.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSE8797B SETGK87W ERROR " SG7-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY "SEI-F01 " SEI-F01 UPON CONS.
     DISPLAY "SEI-F03 " SEI-F03 UPON CONS.
     DISPLAY "SEI-F14 " SEI-F14 UPON CONS.
     DISPLAY "SEI-F04 " SEI-F04 UPON CONS.
     DISPLAY "SEI-F05 " SEI-F05 UPON CONS.
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
     DISPLAY  "*** SSE8797B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     DJSYUKF.
     OPEN     I-O       SETGK87.
     OPEN     OUTPUT    SETGK87W.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         COUNTERS.
     INITIALIZE         FLAGS.
** 請求合計ファイル読込み
     PERFORM  SEI-READ.
*
     IF  END-FLG = "END"
         DISPLAY NC"＃＃対象データ無！！＃＃" UPON CONS
         STOP  RUN
     END-IF.
*
 INIT-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　メイン処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-RTN               SECTION.
*    ＤＣＭ出荷情報ファイル検索
     PERFORM  DJSYUK-READ-SEC.
*
     IF  DJS-INV-FLG  =  SPACE
        MOVE    SPACE    TO    SG7-REC
        INITIALIZE             SG7-REC
        MOVE    SEI-REC  TO    SG7-REC
        MOVE    DJS-F03  TO    SG7-F17
        WRITE   SG7-REC
        ADD     1        TO    SG873-CNT
        MOVE    DJS-F03  TO    SEI-F05
        REWRITE  SEI-REC
     END-IF.
** 請求合計ファイル読込み
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
     CLOSE    DJSYUKF.
     CLOSE    SETGK87W.
*
     DISPLAY "+++ ｾｲｷｭｳｺﾞｳｹｲ INPUT =" RD-CNT  " +++" UPON CONS.
     DISPLAY "+++ ﾀｲｼｮｳﾃﾞｰﾀ　ｹﾝｽｳ  =" IN01-CNT " +++" UPON CONS.
     DISPLAY "+++ ｼｻﾞｲ87     OUT   =" SG873-CNT " +++" UPON CONS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSE8797B END *** "
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
 SEI-READ-010.
     READ     SETGK87    AT   END
              MOVE     "END"      TO   END-FLG
              GO                  TO   SEI-READ-EXIT
              NOT  AT  END
              ADD       1         TO   RD-CNT
     END-READ.
*
     IF  RD-CNT(4:4) = "0000" OR "5000"
         DISPLAY "READ-CNT = " RD-CNT  UPON CONS
     END-IF.
*
     IF  SEI-F01 NOT = 100441
         GO                       TO   SEI-READ
     ELSE
         ADD     1                TO   IN01-CNT
     END-IF.
*
 SEI-READ-EXIT.
     EXIT.
****************************************************************
*　　　　ＤＣＭ出荷実績ファイル検索　　　　　　　　　　　　　　*
****************************************************************
 DJSYUK-READ-SEC               SECTION.
*
     MOVE  SEI-F01          TO    DJS-K03.
     MOVE  SEI-F03          TO    DJS-K05.
     MOVE  SEI-F14          TO    DJS-K08.
     MOVE  SEI-F05          TO    DJS-K06.
     MOVE  01               TO    DJS-K07.
*
     READ     DJSYUKF
         INVALID
              MOVE  "INV"    TO   DJS-INV-FLG
         NOT  INVALID
              MOVE  SPACE    TO   DJS-INV-FLG
     END-READ.
*
 DJSYUKA-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
