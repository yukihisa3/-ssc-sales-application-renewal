****************************************************************
*    顧客名　　　　　　　：　サカタのタネ殿　　　　　　　　　
*    業務名　　　　　　　：　コメリ流通ＢＭＳオンライン
*    モジュール名　　　　：　請求データ削除　　　　　　　　　　
*    作成日／更新日　　　：　2013/03/18
*    作成者／更新者　　　：　ＮＡＶ三浦　　　　　　　　　　　　
*    処理概要　　　　　　：　支払ワーク　　　と請求合計ファイル
*                        ：　の金額を照合し、一致したデータを　
*                        ：　削除する。
*                        ：　SBM0240B流用
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SBMK240B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          13/03/18.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM.
 OBJECT-COMPUTER.       FACOM.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 支払ワーク　　　 >>--*
     SELECT   BMSSIHF  ASSIGN         DA-01-VI-BMSSI2W1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SSI-F013  SSI-F308
                             SSI-F405  SSI-F415  SSI-F402
                        STATUS         SSI-ST.
*----<< 請求合計Ｆ >>--*
     SELECT   SETGKF    ASSIGN         DA-01-VI-SETGKFK3
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SEI-F01   SEI-F03
                                       SEI-F05
                        STATUS         SEI-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 支払ワーク　　　 >>--*
 FD  BMSSIHF           LABEL RECORD   IS   STANDARD.
     COPY     BMSSIHF   OF        XFDLIB
              JOINING   SSI       PREFIX.
*----<< 請求合計Ｆ >>--*
 FD  SETGKF             LABEL RECORD   IS   STANDARD.
     COPY     SETGKFP   OF        XFDLIB
              JOINING   SEI       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
*----<< ﾌﾟﾛｸﾞﾗﾑID >>--*
 01  PG-ID              PIC  X(08)     VALUE  "SBM0240B".
*----<< ﾌﾗｸﾞｴﾘｱ >>--*
 01  END-FLG            PIC  X(03)     VALUE  SPACE.
 01  INV-FLG            PIC  X(01)     VALUE  SPACE.
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SSI-ST             PIC  X(02)     VALUE  SPACE.
 01  SEI-ST             PIC  X(02)     VALUE  SPACE.
*----<< ｶｳﾝﾄｴﾘｱ >>--*
 01  READ-CNT           PIC  9(07)     VALUE  ZERO.
 01  DEL-CNT            PIC  9(07)     VALUE  ZERO.
 01  SKIP-CNT           PIC  9(07)     VALUE  ZERO.
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
 01  WK-SEIKIN          PIC  S9(11).
*----<< 伝票_変換（９桁用） >>--*
 01  WK-DENNO           PIC  9(09).
 01  WK-DENNO-R         REDEFINES     WK-DENNO.
     03  HEN-DENNO      PIC  X(09).
 LINKAGE                SECTION.
 01  PARA-OUT-CNT       PIC  9(07).
****************************************************************
 PROCEDURE              DIVISION  USING  PARA-OUT-CNT.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 支払ワーク　　　 >>--*
 SSI-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      BMSSIHF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID " BMSSIHW1 ERROR " SSI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     4000           TO        PROGRAM-STATUS
     STOP     RUN.
*----<< 請求合計Ｆ >>--*
 SEI-ERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SETGKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID " SETGKFP3  ERROR " SEI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     4000           TO        PROGRAM-STATUS
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
     OPEN     INPUT     BMSSIHF.
     OPEN     I-O       SETGKF.
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
     IF  SSI-F420  =  "-"
         COMPUTE  WK-SEIKIN  =   SSI-F419  *  -1
     ELSE
         MOVE  SSI-F419   TO   WK-SEIKIN
     END-IF.
*  請求合計ファイル検索
     PERFORM  900-SEI-READ.
     IF   INV-FLG    =   SPACE
*  金額一致の場合削除
          IF  WK-SEIKIN  =  SEI-F06
              DELETE  SETGKF
              ADD     1      TO   DEL-CNT
          END-IF
     END-IF.
*  支払ワーク読込み
     PERFORM  900-SSI-READ.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
*ファイルのクローズ
     CLOSE    BMSSIHF.
     CLOSE    SETGKF.
*削除件数表示
     DISPLAY "READ  CNT  = " READ-CNT UPON CONS.
     DISPLAY "SKIP  CNT  = " SKIP-CNT UPON CONS.
     DISPLAY "ｻｸｼﾞｮ ｹﾝｽｳ = " DEL-CNT UPON CONS.
     MOVE   DEL-CNT      TO  PARA-OUT-CNT.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    請求合計Ｆ　　 READ                          *
*--------------------------------------------------------------*
 900-SEI-READ           SECTION.
*
     INITIALIZE                        SEI-REC.
     MOVE     SSI-F013           TO   SEI-F01.
     MOVE     SSI-F405 (1:4)     TO   SEI-F03.
     MOVE     SSI-F402 (1:9)     TO   WK-DENNO.
     MOVE     HEN-DENNO          TO   SEI-F05.
     READ     SETGKF
         INVALID
              MOVE      "E"           TO   INV-FLG
******** DISPLAY  "*********" UPON CONS
*        DISPLAY  "SSI-F013=" SSI-F013 UPON CONS
*        DISPLAY  "SSI-F405 (1:5)" SSI-F405 (1:5) UPON CONS
*        DISPLAY  "SSI-F402 (1:7)" SSI-F402 (1:7) UPON CONS
** TEST
**
         DISPLAY NC"＃消込無⇒取＝" SSI-F013
             " " NC"店＝" SSI-F405 (1:5)
             " " NC"伝＝" SSI-F402 (1:9) UPON CONS
         NOT INVALID
              MOVE      SPACE         TO   INV-FLG
     END-READ.
 900-SEI-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    支払ワーク        READ                       *
*--------------------------------------------------------------*
 900-SSI-READ           SECTION.
*
 900-SSI-READ-010.
*
     READ     BMSSIHF   NEXT
              AT END
              MOVE      "END"         TO   END-FLG
              GO  TO     900-SSI-READ-EXIT
     END-READ.
*
     ADD     1  TO     READ-CNT.
*  店舗ＣＤ判定
     IF  SSI-F405(1:4)  =  "9999"
         ADD   1  TO   SKIP-CNT
         GO  TO    900-SSI-READ-010
     END-IF.
*
 900-SSI-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
