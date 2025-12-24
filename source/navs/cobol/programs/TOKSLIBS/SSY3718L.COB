****************************************************************
*    顧客名　　　　　　　：　_サカタのタネ殿　　　　　　　　　
*    業務名　　　　　　　：　ナフコ新ＥＤＩシステム            *
*    モジュール名　　　　：　受領アンマッチリスト発行　　　　　*
*    作成日／更新日　　　：　12/04/02                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　新受領アンマッチデータを読み、受領*
*                        ：　アンマッチリストを発行する。      *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY3718L.
 AUTHOR.                Y.Y.
 DATE-WRITTEN.          08/11/17.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         YA        IS   YA
         YB        IS   PITCH-1-5
         YB-21     IS   BAIKAKU-1-5
         YA-21     IS   BAIKAKU
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 新受領アンマッチデータ >>--*
     SELECT   NFNUNMF   ASSIGN         DA-01-VI-NFNUNML1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  UNM-F01  UNM-F02  UNM-F03
                        STATUS         UNM-ST.
*----<< ナフコ商品マスタ >>--*
     SELECT   NFSHOMS   ASSIGN         DA-01-VI-NFSHOMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SHO-F01
                        STATUS         SHO-ST.
*----<< 店舗マスタ >>--*
     SELECT   HTENMS    ASSIGN         DA-01-VI-TENMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TEN-F52  TEN-F011
                        STATUS         TEN-ST.
*----<< 作場マスタ >>--*
     SELECT   SAKUBAF   ASSIGN         DA-01-VI-SAKUBAL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SYK-F01
                        STATUS         SYK-ST.
*----<< プリンタ >>-*
     SELECT   PRTF      ASSIGN         LP-04.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 新受領アンマッチデータ >>--*
 FD  NFNUNMF            LABEL RECORD   IS   STANDARD.
     COPY     NFNUNMF   OF        XFDLIB
              JOINING   UNM       PREFIX.
*----<< ナフコ商品マスタ >>--*
 FD  NFSHOMS            LABEL RECORD   IS   STANDARD.
     COPY     NFSHOMS   OF        XFDLIB
              JOINING   SHO       PREFIX.
*----<< 店舗マスタ >>--*
 FD  HTENMS             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*----<< 作場マスタ >>--*
 FD  SAKUBAF            LABEL RECORD   IS   STANDARD.
     COPY     SAKUBAF   OF        XFDLIB
              JOINING   SYK       PREFIX.
*----<< プリンタ >>-*
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC            PIC  X(200).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  X(03)     VALUE  SPACE.
     03  HTENMS-INV-FLG PIC  X(03)     VALUE  SPACE.
     03  NFSHOMS-INV-FLG PIC X(03)     VALUE  SPACE.
     03  SAKUBAF-INV-FLG PIC X(03)     VALUE  SPACE.
     03  PAGE-FLG       PIC  X(01)     VALUE  SPACE.
 01  COUNTERS.
     03  LINE-CNT       PIC  9(03)     VALUE  ZERO.
     03  PAGE-CNT       PIC  9(03)     VALUE  ZERO.
     03  IN-CNT         PIC  9(07)     VALUE  ZERO.
 01  WK-UNM-F01         PIC  9(05)     VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  UNM-ST             PIC  X(02).
 01  SHO-ST             PIC  X(02).
 01  TEN-ST             PIC  X(02).
 01  SYK-ST             PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  PG-ID              PIC  X(08)     VALUE   "SSY3718L".
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
 01  WK-KEI-YMD.
     03  WK-KEI-YYYY    PIC  9(04).
     03  WK-KEI-KU1     PIC  X(01).
     03  WK-KEI-MM      PIC  9(02).
     03  WK-KEI-KU2     PIC  X(01).
     03  WK-KEI-DD      PIC  9(02).
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HD01.
     03  FILLER         CHARACTER TYPE BAIKAKU-1-5.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD01-PGID  PIC  X(08).
         05  FILLER     PIC  X(38)     VALUE  SPACE.
         05  FILLER     PIC  N(12)     VALUE
                        NC"＜受領アンマッチリスト＞".
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER     PIC  X(30)     VALUE  SPACE.
         05  HD01-YYYY  PIC  9999.
         05  FILLER     PIC  N(01)     VALUE  NC"年".
         05  HD01-MM    PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"月".
         05  HD01-DD    PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"日".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  HD01-PAGE  PIC  ZZ9.
         05  FILLER     PIC  N(01)     VALUE  NC"頁".
 01  HD02               CHARACTER TYPE YA.
     03  FILLER         PIC  X(01)     VALUE  SPACE.
     03  FILLER         PIC  X(31)     VALUE
         "<----------------------------- ".
     03  FILLER         PIC  N(07)     VALUE
         NC"受　領　情　報".
     03  FILLER         PIC  X(37)     VALUE
         " ------------------------> ! <------ ".
     03  FILLER         PIC  N(07)     VALUE
         NC"出　荷　情　報".
     03  FILLER         PIC  X(17)     VALUE
         " ------->  <---- ".
     03  FILLER         PIC  N(07)     VALUE
         NC"エラー　情　報".
     03  FILLER         PIC  X(07)     VALUE  " ----->".
*
 01  HD03               CHARACTER TYPE YA.
     03  FILLER         PIC  X(02)     VALUE  SPACE.
     03  FILLER         PIC  N(02)     VALUE  NC"店舗".
     03  FILLER         PIC  X(03)     VALUE  SPACE.
     03  FILLER         PIC  N(03)     VALUE  NC"計上日".
     03  FILLER         PIC  X(04)     VALUE  SPACE.
     03  FILLER         PIC  N(04)     VALUE  NC"伝票番号".
     03  FILLER         PIC  X(01)     VALUE  SPACE.
     03  FILLER         PIC  N(04)     VALUE  NC"相手商品".
     03  FILLER         PIC  X(06)     VALUE  SPACE.
     03  FILLER         PIC  N(03)     VALUE  NC"商品名".
     03  FILLER         PIC  X(25)     VALUE  SPACE.
     03  FILLER         PIC  X(01)     VALUE  "!".
     03  FILLER         PIC  X(03)     VALUE  SPACE.
     03  FILLER         PIC  N(03)     VALUE  NC"納品日".
     03  FILLER         PIC  X(03)     VALUE  SPACE.
     03  FILLER         PIC  N(04)     VALUE  NC"相手商品".
*
 01  HD04               CHARACTER TYPE YA.
     03  FILLER         PIC  X(02)     VALUE  SPACE.
     03  FILLER         PIC  N(03)     VALUE  NC"店舗名".
     03  FILLER         PIC  X(35)     VALUE  SPACE.
     03  FILLER         PIC  N(04)     VALUE  NC"受領数量".
     03  FILLER         PIC  X(02)     VALUE  SPACE.
     03  FILLER         PIC  N(04)     VALUE  NC"受領原価".
     03  FILLER         PIC  X(02)     VALUE  SPACE.
     03  FILLER         PIC  N(04)     VALUE  NC"受領売価".
     03  FILLER         PIC  X(02)     VALUE  SPACE.
     03  FILLER         PIC  X(01)     VALUE  "!".
     03  FILLER         PIC  X(02)     VALUE  SPACE.
     03  FILLER         PIC  N(04)     VALUE  NC"出荷数量".
     03  FILLER         PIC  X(02)     VALUE  SPACE.
     03  FILLER         PIC  N(04)     VALUE  NC"出荷原価".
     03  FILLER         PIC  X(02)     VALUE  SPACE.
     03  FILLER         PIC  N(04)     VALUE  NC"出荷売価".
*
 01  HD05               CHARACTER TYPE YA.
     03  FILLER         PIC  X(07)     VALUE  SPACE.
     03  FILLER         PIC  N(04)     VALUE  NC"伝票区分".
     03  FILLER         PIC  X(02)     VALUE  SPACE.
     03  FILLER         PIC  N(04)     VALUE  NC"受領区分".
     03  FILLER         PIC  X(02)     VALUE  SPACE.
     03  FILLER         PIC  N(02)     VALUE  NC"理由".
     03  FILLER         PIC  X(42)     VALUE  SPACE.
     03  FILLER         PIC  X(01)     VALUE  "!".
*
 01  HD06               CHARACTER TYPE YA.
     03  FILLER         PIC  X(07)     VALUE  SPACE.
     03  FILLER         PIC  N(04)     VALUE  NC"連絡事項".
     03  FILLER         PIC  X(58)     VALUE  SPACE.
     03  FILLER         PIC  X(01)     VALUE  "!".
*
 01  SEN01.
     03  FILLER         PIC  X(136)    VALUE  ALL "=".
*
 01  SEN02.
     03  FILLER         PIC  X(07)     VALUE  SPACE.
     03  FILLER         PIC  X(128)    VALUE  ALL "-".
*
 01  SEN03.
     03  FILLER         PIC  X(136)    VALUE  ALL "-".
*
 01  MS01               CHARACTER TYPE PITCH-1-5.
     03  FILLER         PIC  X(01)     VALUE  SPACE.
     03  MS01-TENCD     PIC  9(05).
     03  FILLER         PIC  X(01)     VALUE  SPACE.
     03  MS01-JYUDT     PIC  X(10).
     03  FILLER         PIC  X(01)     VALUE  SPACE.
     03  MS01-DENNO     PIC  X(10).
     03  FILLER         PIC  X(01)     VALUE  SPACE.
     03  MS01-JAN       PIC  X(13).
     03  FILLER         PIC  X(01)     VALUE  SPACE.
     03  MS01-SYONM     PIC  X(30).
*****03  FILLER         PIC  X(01)     VALUE  SPACE.
     03  MS01-KU        PIC  X(01).
     03  FILLER         PIC  X(01)     VALUE  SPACE.
     03  MS01-SYUDT     PIC  X(10).
     03  FILLER         PIC  X(01)     VALUE  SPACE.
     03  MS01-SYUSYOCD  PIC  X(13).
     03  FILLER         PIC  X(01)     VALUE  SPACE.
     03  MS01-SYUKABA   PIC  X(02).
     03  FILLER         PIC  X(06)     VALUE  SPACE.
     03  MS01-ERR1      PIC  N(08).
     03  FILLER         PIC  X(02)     VALUE  SPACE.
     03  MS01-ERR2      PIC  N(08).
*
 01  MS02               CHARACTER TYPE PITCH-1-5.
     03  FILLER         PIC  X(01)     VALUE  SPACE.
     03  MS02-TENNM     PIC  N(10).
     03  FILLER         PIC  X(26)     VALUE  SPACE.
     03  MS02-JYUSU     PIC  -,---,--9.
     03  FILLER         PIC  X(01)     VALUE  SPACE.
     03  MS02-JYUGEN    PIC  -,---,--9.
     03  FILLER         PIC  X(01)     VALUE  SPACE.
     03  MS02-JYUBAI    PIC  -,---,--9.
     03  FILLER         PIC  X(02)     VALUE  SPACE.
     03  MS02-KU        PIC  X(01).
     03  FILLER         PIC  X(01)     VALUE  SPACE.
     03  MS02-SYUSU     PIC  -,---,--9.
     03  FILLER         PIC  X(01)     VALUE  SPACE.
     03  MS02-SYUGEN    PIC  -,---,--9.
     03  FILLER         PIC  X(01)     VALUE  SPACE.
     03  MS02-SYUBAI    PIC  -,---,--9.
     03  FILLER         PIC  X(04)     VALUE  SPACE.
     03  MS01-ERR3      PIC  N(08).
     03  FILLER         PIC  X(02)     VALUE  SPACE.
     03  MS01-ERR4      PIC  N(08).
*
 01  MS03               CHARACTER TYPE PITCH-1-5.
     03  FILLER         PIC  X(07)     VALUE  SPACE.
     03  MS03-DENKU     PIC  99.
     03  FILLER         PIC  X(01)     VALUE  SPACE.
     03  MS03-DENKUNM   PIC  N(04).
     03  FILLER         PIC  X(01)     VALUE  SPACE.
     03  MS03-JYUKU     PIC  99.
     03  FILLER         PIC  X(01)     VALUE  SPACE.
     03  MS03-JYUNM     PIC  N(04).
     03  FILLER         PIC  X(01)     VALUE  SPACE.
     03  MS03-RIYU      PIC  9.
     03  MS03-RIYUCD    PIC  9.
     03  FILLER         PIC  X(01)     VALUE  SPACE.
     03  MS03-RIYUNM    PIC  N(18).
     03  FILLER         PIC  X(16)     VALUE  SPACE.
     03  MS03-KU        PIC  X(01).
     03  FILLER         PIC  X(34)     VALUE  SPACE.
     03  MS01-ERR5      PIC  N(08).
     03  FILLER         PIC  X(02)     VALUE  SPACE.
     03  MS01-ERR6      PIC  N(08).
*
 01  MS04               CHARACTER TYPE PITCH-1-5.
     03  FILLER         PIC  X(07)     VALUE  SPACE.
     03  MS04-RENRAKU1  PIC  N(02).
     03  MS04-RENRAKU2  PIC  N(26).
     03  MS04-RENRAKU3  PIC  N(02).
     03  FILLER         PIC  X(21)     VALUE  SPACE.
     03  MS04-KU        PIC  X(01).
     03  FILLER         PIC  X(34)     VALUE  SPACE.
     03  MS04-ERR5      PIC  N(08).
     03  FILLER         PIC  X(02)     VALUE  SPACE.
     03  MS04-ERR6      PIC  N(08).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
*
 01  WK-DATE8.
     03  WK-Y                PIC  9(04)  VALUE  ZERO.
     03  WK-M                PIC  9(02)  VALUE  ZERO.
     03  WK-D                PIC  9(02)  VALUE  ZERO.
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 新受領アンマッチデータ >>--*
 NFNUNMF-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      NFNUNMF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### " PG-ID " NFNUNML1  ERROR " UNM-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< ナフコ商品マスタ >>--*
 NFSHOMS-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      NFSHOMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### " PG-ID "  NFSHOMS1 ERROR " SHO-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 店舗マスタ >>--*
 HTENMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTENMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### " PG-ID " HTENMS   ERROR " TEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 作場マスタ >>--*
 SAKUBAF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SAKUBAF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### " PG-ID " SAKUBAF  ERROR " SYK-ST " "
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
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYS-DATE            TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   WK-DATE8.
     DISPLAY  "*** " PG-ID " START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*プログラムコントロール
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     END-FLG   =    "END".
     PERFORM  300-END-RTN.
*プログラム終了メッセージ
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** " PG-ID " END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
*ファイルのオープン
     OPEN     INPUT     NFNUNMF.
     OPEN     INPUT     NFSHOMS.
     OPEN     INPUT     HTENMS.
     OPEN     INPUT     SAKUBAF.
     OPEN     OUTPUT    PRTF.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         FLAGS.
     INITIALIZE         COUNTERS.
     MOVE     "1"            TO   PAGE-FLG.
     MOVE     99             TO   LINE-CNT.
*新受領アンマッチデータ読込
     PERFORM  900-UNM-READ.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*行カウンタが６０以上になったら
     IF  LINE-CNT  >  60
         PERFORM 210-HEAD-PRINT
     END-IF.
*店舗ＣＤがブレイクしたら
     IF  UNM-F01  =  WK-UNM-F01
     AND PAGE-FLG =  SPACE
         WRITE  PRT-REC  FROM  SEN02 AFTER 1
         ADD     1                TO      LINE-CNT
         MOVE    SPACE            TO      MS01  MS02  MS03  MS04
      ELSE
         MOVE    SPACE            TO      MS01  MS02  MS03  MS04
         MOVE    137607           TO      TEN-F52
         MOVE    UNM-F01          TO      TEN-F011  MS01-TENCD
         PERFORM  900-TEN-READ
         IF  HTENMS-INV-FLG = SPACE
             MOVE  TEN-F03        TO      MS02-TENNM
         ELSE
             MOVE  ALL NC"＊"     TO      MS02-TENNM
         END-IF
         MOVE    UNM-F01          TO      WK-UNM-F01
         IF  PAGE-FLG = SPACE
             WRITE  PRT-REC  FROM  SEN03 AFTER 1
         END-IF
         ADD     1                TO      LINE-CNT
         MOVE    SPACE            TO      PAGE-FLG
      END-IF.
*　伝票番号
      MOVE       UNM-F02          TO      MS01-DENNO.
*明細行をセット
      IF   UNM-F26  NOT =  "1"
           GO                     TO      MAIN010
      END-IF.
*　受領計上日
      MOVE       UNM-F04(1:4)     TO      WK-KEI-YYYY.
      MOVE       UNM-F04(5:2)     TO      WK-KEI-MM.
      MOVE       UNM-F04(7:2)     TO      WK-KEI-DD.
      MOVE       "/"              TO      WK-KEI-KU1  WK-KEI-KU2.
      MOVE       WK-KEI-YMD       TO      MS01-JYUDT.
*　伝票番号
      MOVE       UNM-F02          TO      MS01-DENNO.
*　受領商品ＣＤ
      MOVE       UNM-F05          TO      MS01-JAN.
*　受領商品名称
      MOVE       UNM-F05          TO      SHO-F01.
      PERFORM  900-SHO-READ.
      IF  NFSHOMS-INV-FLG = SPACE
          MOVE   SHO-F07          TO      MS01-SYONM(1:20)
          MOVE   SHO-F08(1:10)    TO      MS01-SYONM(21:10)
      ELSE
          MOVE   ALL "*"          TO      MS01-SYONM
      END-IF.
*　受領数量
      MOVE       UNM-F07          TO      MS02-JYUSU.
*　受領原価
      MOVE       UNM-F08          TO      MS02-JYUGEN.
*　受領売価
      MOVE       UNM-F09          TO      MS02-JYUBAI.
*　伝票区分
      MOVE       UNM-F06          TO      MS03-DENKU.
*　伝票区分名
      EVALUATE   UNM-F06
          WHEN    1   MOVE NC"仕入通"  TO MS03-DENKUNM
          WHEN    2   MOVE NC"仕入Ｆ"  TO MS03-DENKUNM
          WHEN   11   MOVE NC"返品　"  TO MS03-DENKUNM
          WHEN   12   MOVE NC"値引　"  TO MS03-DENKUNM
          WHEN   61   MOVE NC"相殺　"  TO MS03-DENKUNM
          WHEN  OTHER MOVE ALL NC"＊"  TO MS03-DENKUNM
      END-EVALUATE.
*　受領区分名
      MOVE       UNM-F10          TO      MS03-JYUKU.
      EVALUATE   UNM-F10
         WHEN    0  MOVE NC"返品値引"  TO MS03-JYUNM
         WHEN    1  MOVE NC"予定通り"  TO MS03-JYUNM
         WHEN    2  MOVE NC"訂正受領"  TO MS03-JYUNM
         WHEN    3  MOVE NC"仕訂赤黒"  TO MS03-JYUNM
         WHEN OTHER MOVE ALL NC"＊"    TO MS03-JYUNM
      END-EVALUATE.
*　理由
      MOVE       UNM-F12          TO      MS03-RIYU.
*　理由ＣＤ
      MOVE       UNM-F13          TO      MS03-RIYUCD.
*　理由名
      IF  UNM-F14 NOT = SPACE
          MOVE       UNM-F14      TO      MS03-RIYUNM
      ELSE
          MOVE NC"　　　　　　　　　　" TO MS03-RIYUNM
      END-IF.
*　連絡事項
      MOVE       NC"（"           TO      MS04-RENRAKU1.
      MOVE       UNM-F15          TO      MS04-RENRAKU2.
      MOVE       NC"）"           TO      MS04-RENRAKU3.
 MAIN010.
*明細行をセット
      IF   UNM-F27  NOT =  "1"
           GO                     TO      MAIN020
      END-IF.
*　出荷計上日
      MOVE       UNM-F20(1:4)     TO      WK-KEI-YYYY.
      MOVE       UNM-F20(5:2)     TO      WK-KEI-MM.
      MOVE       UNM-F20(7:2)     TO      WK-KEI-DD.
      MOVE       "/"              TO      WK-KEI-KU1  WK-KEI-KU2.
      MOVE       WK-KEI-YMD       TO      MS01-SYUDT.
*　相手商品
      MOVE       UNM-F21          TO      MS01-SYUSYOCD.
*　出荷数量
      MOVE       UNM-F23          TO      MS02-SYUSU.
*　出荷原価
      MOVE       UNM-F24          TO      MS02-SYUGEN.
*　出荷売価
      MOVE       UNM-F25          TO      MS02-SYUBAI.
*　出荷場所
      MOVE       UNM-F22          TO      MS01-SYUKABA.
*　出荷場所名
***** MOVE       UNM-F22          TO      SYK-F01.
*     PERFORM  900-SYK-READ.
*     IF  SAKUBAF-INV-FLG = SPACE
*         MOVE   SYK-F02          TO      MS01-SYUKABANM
*     ELSE
*         MOVE   ALL "*"          TO      MS01-SYUKABANM
******END-IF.
 MAIN020.
*
      IF  UNM-F28 = "1"
          MOVE   NC"出荷データ無　　" TO  MS01-ERR1
      END-IF.
*
      IF  UNM-F29 = "1"
          MOVE   NC"受領データ無　　" TO  MS01-ERR2
      END-IF.
*
      IF  UNM-F30 = "1"
          MOVE   NC"数量違い　　　　" TO  MS01-ERR2
      END-IF.
*
      IF  UNM-F31 = "1"
          MOVE   NC"原価単価違い　　" TO  MS01-ERR2
      END-IF.
*
      IF  UNM-F32 = "1"
          MOVE   NC"ナフコ側記票伝票" TO  MS01-ERR2
      END-IF.
*
      IF  UNM-F33 = "1"
          MOVE   NC"計上日違い　　　" TO  MS01-ERR2
      END-IF.
*
      MOVE   "!"                      TO  MS01-KU.
      MOVE   "!"                      TO  MS02-KU.
      MOVE   "!"                      TO  MS03-KU.
      MOVE   "!"                      TO  MS04-KU.
*　明細行印字
      WRITE  PRT-REC  FROM  MS01  AFTER 1.
      WRITE  PRT-REC  FROM  MS02  AFTER 1.
      WRITE  PRT-REC  FROM  MS03  AFTER 1.
      WRITE  PRT-REC  FROM  MS04  AFTER 1.
*　行番号カウント
      ADD    4                        TO  LINE-CNT.
*新受領アンマッチデータ読込
     PERFORM  900-UNM-READ.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
*ファイルのクローズ
     CLOSE    NFNUNMF.
     CLOSE    NFSHOMS.
     CLOSE    HTENMS.
     CLOSE    SAKUBAF.
     CLOSE    PRTF.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL 4       ﾀｲﾄﾙ ﾌﾟﾘﾝﾄ ｼｮﾘ                              *
*--------------------------------------------------------------*
 210-HEAD-PRINT         SECTION.
*ＨＥＡＤ印字
     IF       PAGE-CNT  >    0
              MOVE   SPACE   TO   PRT-REC
              WRITE  PRT-REC  AFTER  PAGE
     END-IF.
*
     ADD      1              TO   PAGE-CNT.
*
     MOVE     "SSY3718L"     TO   HD01-PGID.
     MOVE     WK-Y           TO   HD01-YYYY.
     MOVE     WK-M           TO   HD01-MM.
     MOVE     WK-D           TO   HD01-DD.
     MOVE     PAGE-CNT       TO   HD01-PAGE.
     MOVE     "1"            TO   PAGE-FLG.
*
     WRITE    PRT-REC   FROM      HD01      AFTER     2.
     WRITE    PRT-REC   FROM      SEN01     AFTER     3.
     WRITE    PRT-REC   FROM      HD02      AFTER     1.
     WRITE    PRT-REC   FROM      HD03      AFTER     1.
     WRITE    PRT-REC   FROM      HD04      AFTER     1.
     WRITE    PRT-REC   FROM      HD05      AFTER     1.
     WRITE    PRT-REC   FROM      HD06      AFTER     1.
     WRITE    PRT-REC   FROM      SEN01     AFTER     1.
     MOVE     11             TO   LINE-CNT.
*
 210-HEAD-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    新受領アンマッチデータ読込　　　　　　　　　 *
*--------------------------------------------------------------*
 900-UNM-READ           SECTION.
*
     READ     NFNUNMF
         AT END
              MOVE      "END"          TO   END-FLG
              GO                       TO   900-UNM-READ-EXIT
     END-READ.
*
     MOVE     1                        TO   IN-CNT
*
     IF  IN-CNT(5:3) = "000" OR "500"
         DISPLAY "READ-CNT = " IN-CNT  UPON CONS
     END-IF.
*
 900-UNM-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    店舗マスタ　　READ                           *
*--------------------------------------------------------------*
 900-TEN-READ           SECTION.
*
     READ     HTENMS    INVALID
              MOVE     "INV"      TO   HTENMS-INV-FLG
              NOT  INVALID
              MOVE     SPACE      TO   HTENMS-INV-FLG
     END-READ.
*
 900-TEN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    店舗マスタ　　READ                           *
*--------------------------------------------------------------*
 900-SHO-READ           SECTION.
*
     READ     NFSHOMS   INVALID
              MOVE     "INV"      TO   NFSHOMS-INV-FLG
              NOT  INVALID
              MOVE     SPACE      TO   NFSHOMS-INV-FLG
     END-READ.
*
 900-TEN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    店舗マスタ　　READ                           *
*--------------------------------------------------------------*
 900-SYK-READ           SECTION.
*
     READ     SAKUBAF   INVALID
              MOVE     "INV"      TO   SAKUBAF-INV-FLG
              NOT  INVALID
              MOVE     SPACE      TO   SAKUBAF-INV-FLG
     END-READ.
*
 900-SYK-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
