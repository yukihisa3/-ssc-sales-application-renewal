# SSI0330B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SSI0330B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　支払照合　ホーマーグリーン        *
*    モジュール名　　　　：　請求データ消し込み　　　　　　　　*
*    作成日／更新日　　　：　00/08/11                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　支払明細ファイルと請求明細ファイル*
*                        ：　の金額を照合し、一致したデータを　*
*                            削除する。                        *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSI0330B.
 AUTHOR.                Y.Y.
 DATE-WRITTEN.          00/08/11.
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
*----<< 支払合計ファイル >>--*
     SELECT   SITGKF39   ASSIGN         DA-01-S-SITGKF39
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SSI-ST.
*----<< 請求合計Ｆ >>--*
     SELECT   SEIGK39   ASSIGN         DA-01-VI-SEIGK391
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SEI-F01   SEI-F05
                        STATUS         SEI-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 支払合計ファイル >>--*
 FD  SITGKF39            LABEL RECORD   IS   STANDARD
                        BLOCK CONTAINS  1   RECORDS.
     COPY     SITGKF39   OF        XFDLIB
              JOINING   SSI       PREFIX.
*----<< 請求合計Ｆ >>--*
 FD  SEIGK39            LABEL RECORD   IS   STANDARD.
     COPY     SEIGK39   OF        XFDLIB
              JOINING   SEI       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
*----<< ﾌﾟﾛｸﾞﾗﾑID >>--*
 01  PG-ID              PIC  X(08)     VALUE  "SSI0330B".
*----<< ﾌﾗｸﾞｴﾘｱ >>--*
 01  END-FLG            PIC  X(03)     VALUE  SPACE.
 01  INV-FLG            PIC  X(01)     VALUE  SPACE.
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SSI-ST             PIC  X(02)     VALUE  SPACE.
 01  SEI-ST             PIC  X(02)     VALUE  SPACE.
*----<< ｶｳﾝﾄｴﾘｱ >>--*
 01  DEL-CNT            PIC  9(05)     VALUE  ZERO.
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
*----<< 伝票_変換（９桁用） >>--*
 01  WK-DENNO           PIC  9(11).
 01  WK-DENNO-R         REDEFINES     WK-DENNO.
     03  FILLER         PIC  X(02).
     03  HEN-DENNO      PIC  X(09).
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 支払合計ファイル >>--*
 SYO-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SITGKF39.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID " SITGKF39F  ERROR " SSI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 請求合計Ｆ >>--*
 SEI-ERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SEIGK39.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID " SEIGK39   ERROR " SEI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
*プログラム開始メッセージ
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** " PG-ID " START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*プログラムコントロール
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     END-FLG  NOT =  SPACE.
     PERFORM  300-END-RTN.
*プログラム終了メッセージ
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** " PG-ID " END   *** "
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
     OPEN     INPUT     SITGKF39.
     OPEN     I-O       SEIGK39.
*照合ファイル初期ＲＥＡＤ
     PERFORM  900-SSI-READ.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*
     MOVE     881            TO   SEI-F01.
     MOVE     SSI-F08        TO   WK-DENNO.
     MOVE     HEN-DENNO      TO   SEI-F05.
     PERFORM  900-SEI-READ.
     IF       INV-FLG    =   SPACE
*             金額一致の場合削除
              IF  SSI-F15    =   SEI-F06
                  DELETE  SEIGK39
                  ADD     1      TO   DEL-CNT
              END-IF
     ELSE
              CONTINUE
     END-IF.
*
     PERFORM  900-SSI-READ.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
*ファイルのクローズ
     CLOSE    SITGKF39.
     CLOSE    SEIGK39.
*削除件数表示
     DISPLAY "ｻｸｼﾞｮ ｹﾝｽｳ = " DEL-CNT UPON CONS.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    請求合計Ｆ　　 READ                          *
*--------------------------------------------------------------*
 900-SEI-READ           SECTION.
     READ     SEIGK39
         INVALID
              MOVE      "E"           TO   INV-FLG
         NOT INVALID
              MOVE      SPACE         TO   INV-FLG
     END-READ.
 900-SEI-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    支払明細ファイル　READ                       *
*--------------------------------------------------------------*
 900-SSI-READ           SECTION.
     READ     SITGKF39
              AT END
              MOVE      "END"         TO   END-FLG
     END-READ.
 900-SSI-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
