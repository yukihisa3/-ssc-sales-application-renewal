# SSI7804B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSI7804B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　ＲＨＣ　支払照合                  *
*    モジュール名　　　　：　請求データ消し込み　　　　　　　　*
*    作成日／更新日　　　：　08/12/12                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　支払明細ファイルと請求明細ファイル*
*                        ：　の金額を照合し、一致したデータを　*
*                            削除する。                        *
*    作成日／更新日　　　：　00/00/00                          *
*    作成者／更新者　　　：　　　　　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSI7804B.
 AUTHOR.                Y.Y.
 DATE-WRITTEN.          08/12/12.
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
*----<< 支払明細ファイル >>--*
     SELECT   ROYSMEPF  ASSIGN         DA-01-VI-ROYSMEL3
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SSI-F01  SSI-F02  SSI-F04
                        STATUS         SSI-ST.
*----<< 請求合計Ｆ >>--*
     SELECT   SETGKFT   ASSIGN         DA-01-VI-SETGKFT1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SEI-F01   SEI-F05
                        STATUS         SEI-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 支払明細ファイル >>--*
 FD  ROYSMEPF           LABEL RECORD   IS   STANDARD
                        BLOCK CONTAINS  4   RECORDS.
     COPY     ROYSMEPF  OF        XFDLIB
              JOINING   SSI       PREFIX.
*----<< 請求合計Ｆ >>--*
 FD  SETGKFT            LABEL RECORD   IS   STANDARD.
     COPY     SETGKFT   OF        XFDLIB
              JOINING   SEI       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
*----<< ﾌﾟﾛｸﾞﾗﾑID >>--*
 01  PG-ID              PIC  X(08)     VALUE  "SSI7804B".
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
*----<< 取引先ＣＤ(固定値)  >>--*
 01  WK-TORICD          PIC  9(08)    VALUE 51649.
*----<< 伝票_変換（９桁用） >>--*
 01  WK-DENNO           PIC  9(09).
 01  WK-DENNO-R         REDEFINES     WK-DENNO.
     03  HEN-DENNO      PIC  X(09).
*
 LINKAGE                SECTION.
 01  PARA-OUT-CNT       PIC  9(07).
 01  PARA-SIMEBI        PIC  9(08).
****************************************************************
 PROCEDURE              DIVISION  USING  PARA-OUT-CNT
                                         PARA-SIMEBI.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 支払合計ファイル >>--*
 SYO-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      ROYSMEPF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID " ROYSMEPF ERROR " SSI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 請求合計Ｆ >>--*
 SEI-ERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SETGKFT.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID " SETGKFT   ERROR " SEI-ST " "
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
     OPEN     INPUT     ROYSMEPF.
     OPEN     I-O       SETGKFT.
*
     MOVE     SPACE     TO   SSI-REC.
     INITIALIZE              SSI-REC.
     MOVE     PARA-SIMEBI TO SSI-F01.
*
     START  ROYSMEPF KEY  IS  >=  SSI-F01  SSI-F02  SSI-F04
            INVALID
            DISPLAY NC"＃対象データ無し" UPON CONS
            STOP  RUN
     END-START.
*照合ファイル初期ＲＥＡＤ
     PERFORM  900-SSI-READ.
*
     IF   END-FLG = "END"
            DISPLAY NC"＃対象データ無し" UPON CONS
            STOP  RUN
     END-IF.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*
     MOVE     SSI-F04        TO   WK-DENNO.
     MOVE     WK-TORICD      TO   SEI-F01.
     MOVE     WK-DENNO-R     TO   SEI-F05.
     PERFORM  900-SEI-READ
     IF       INV-FLG    =   SPACE
*             金額一致の場合削除
              IF  SSI-F12    =   SEI-F06
                  DELETE  SETGKFT
                  ADD     1      TO   DEL-CNT
              END-IF
     ELSE
              CONTINUE
     END-IF.
*
     PERFORM  900-SSI-READ.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
*ファイルのクローズ
     CLOSE    ROYSMEPF.
     CLOSE    SETGKFT.
*削除件数表示
     DISPLAY "ｻｸｼﾞｮ ｹﾝｽｳ = " DEL-CNT UPON CONS.
     MOVE   DEL-CNT      TO  PARA-OUT-CNT.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    請求合計Ｆ　　 READ                          *
*--------------------------------------------------------------*
 900-SEI-READ           SECTION.
     READ     SETGKFT
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
     READ     ROYSMEPF
              AT END
              MOVE      "END"         TO   END-FLG
              GO                      TO   900-SSI-READ-EXIT
     END-READ.
*
     IF       SSI-F01  >  PARA-SIMEBI
              MOVE      "END"         TO   END-FLG
     END-IF.
*
 900-SSI-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
