****************************************************************
*    顧客名　　　　　　　：　■サカタのタネ殿　　　　　　　　　*
*    業務名　　　　　　　：　リカバリ処理　　　　　　　　      *
*    モジュール名　　　　：　支払データ削除　　　　　　　　　　*
*    作成日／更新日　　　：　08/06/16                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　支払ヘッダ・明細・テイルの指定締　*
*                        ：　日データを削除する。　　　　　　　*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSI8799B.
 AUTHOR.                Y.Y.
 DATE-WRITTEN.          07/05/28.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 支払情報データ >>--*
     SELECT   SITME871  ASSIGN         DA-01-VI-SITME871
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SIM-F01  SIM-F02
                                       SIM-F09  SIM-F03
                                       SIM-F04  SIM-F05
                        STATUS         SIM-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 支払合計ファイル >>--*
 FD  SITME871           LABEL RECORD   IS   STANDARD.
     COPY     SITME87   OF        XFDLIB
              JOINING   SIM       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  COUNTERS.
     03  SITME-CNT      PIC  9(07)  VALUE  ZERO.
 01  FLGS.
     03  END-FLG1       PIC  X(03)     VALUE  SPACE.
 01  ID-PROGRAM.
     03  PG-ID          PIC  X(08)     VALUE  "SSI8799B".
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SIM-ST             PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
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
 01  WK-DATE            PIC  9(06).
 01  FILLER             REDEFINES      WK-DATE.
     03  WK-YY          PIC  9(02).
     03  WK-MM          PIC  9(02).
     03  WK-DD          PIC  9(02).
*
 01  SYS-DATEW          PIC  9(08).
 01  FILLER             REDEFINES      SYS-DATEW.
     03  SYS-YYW        PIC  9(04).
     03  SYS-MMW        PIC  9(02).
     03  SYS-DDW        PIC  9(02).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD8           PIC 9(08).
*
 LINKAGE                SECTION.
 01  PARA-SIMEBI             PIC 9(08).
****************************************************************
 PROCEDURE              DIVISION   USING  PARA-SIMEBI.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 支払情報データ >>--*
 SIM-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SITME871.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID "  SITME871  ERROR " SIM-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
*スタートメッセージ
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "***   " PG-ID " START  *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     END-FLG1 = "END".
     PERFORM  300-END-RTN.
*
*終了メッサージ出力
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "***   " PG-ID " END    *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
*ファイルのオープン
     OPEN     I-O       SITME871.
*
     DISPLAY "SIMEBI = " PARA-SIMEBI  UPON CONS.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*明細Ｆスタート
     MOVE  SPACE        TO     SIM-REC.
     INITIALIZE                SIM-REC.
     MOVE  PARA-SIMEBI  TO     SIM-F01.
     START SITME871 KEY IS >=  SIM-F01  SIM-F02  SIM-F09
                               SIM-F03  SIM-F04  SIM-F05
           INVALID
           DISPLAY NC"対象データなし明細" UPON CONS
           MOVE "END"   TO     END-FLG1
     END-START.
*
     IF  END-FLG1 NOT = "END"
         PERFORM SITME871-READ-SEC  UNTIL  END-FLG1 = "END"
     END-IF.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
*
*ファイルのクローズ
     CLOSE    SITME871.
*
     DISPLAY "* SITME871 DEL-CNT = " SITME-CNT UPON CONS.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    ヘッダＦ読込削除
*--------------------------------------------------------------*
 SITME871-READ-SEC      SECTION.
*
     READ     SITME871
              NEXT  AT  END
              MOVE  "END"     TO   END-FLG1
              GO              TO   SITME871-READ-EXIT
     END-READ.
*
     IF       SIM-F01  NOT =  PARA-SIMEBI
              MOVE  "END"     TO   END-FLG1
              GO              TO   SITME871-READ-EXIT
     END-IF.
*
     DELETE   SITME871.
*
     ADD      1               TO   SITME-CNT.
*
 SITME871-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
