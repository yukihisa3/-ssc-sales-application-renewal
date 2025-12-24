****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　リック受信件数リスト　　　　　　　*
*    モジュール名　　　　：　リック受信件数リスト　　　　　　　*
*    作成日／更新日　　　：　2008/04/21                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　資材植物データを振り分ける。　　　*
*                        ：　                                  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SJH8910L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          07/05/15.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
     YA            IS        CHR-2
     YB-21         IS        CHR-21
     YB            IS        CHR-15
     CONSOLE       IS        CONS
     STATION       IS        STAT.
****************************************************************
 INPUT-OUTPUT              SECTION.
****************************************************************
 FILE-CONTROL.
*----<<リック（資材）>>----*
     SELECT   LICFILE1   ASSIGN         DA-01-S-LICFILE1
                        ORGANIZATION    SEQUENTIAL
                        STATUS          LC1-ST.
*----<<リック（植物）>>----*
     SELECT   LICFILE2   ASSIGN         DA-01-S-LICFILE2
                        ORGANIZATION    SEQUENTIAL
                        STATUS          LC2-ST.
*----<<リック（以外）>>----*
     SELECT   LICFILE3   ASSIGN         DA-01-S-LICFILE3
                        ORGANIZATION    SEQUENTIAL
                        STATUS          LC3-ST.
*----<<プリント>>----*
     SELECT   PRTFILE   ASSIGN  TO      LP-04-PRTF
                        FILE    STATUS  PRT-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<<リック（資材）>>----*
 FD  LICFILE1
                        BLOCK CONTAINS 1 RECORDS.
 01  LC1-REC            PIC  X(3141).
*----<<リック（植物）>>----*
 FD  LICFILE2
                        BLOCK CONTAINS 1 RECORDS.
 01  LC2-REC            PIC  X(3141).
*----<<リック（以外）>>----*
 FD  LICFILE3
                        BLOCK CONTAINS 1 RECORDS.
 01  LC3-REC            PIC  X(3141).
*----<<プリントファイル>>----*
 FD  PRTFILE
     LABEL       RECORD    IS        OMITTED.
 01  PRT-REC.
     03  FILLER            PIC X(200).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  LC1-FLG        PIC  X(03)   VALUE SPACE.
     03  LC2-FLG        PIC  X(03)   VALUE SPACE.
     03  LC3-FLG        PIC  X(03)   VALUE SPACE.
     03  CHK-FLG        PIC  X(03)   VALUE SPACE.
 01  WK-CNT.
     03  LC1-CNT        PIC  9(07)   VALUE ZERO.
     03  LC2-CNT        PIC  9(07)   VALUE ZERO.
     03  LC3-CNT        PIC  9(07)   VALUE ZERO.
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
     03  LC1-ST         PIC  X(02).
     03  LC2-ST         PIC  X(02).
     03  LC3-ST         PIC  X(02).
     03  PRT-ST         PIC  X(02).
*
 01  PG-ID             PIC  X(08)      VALUE  "SJH8910L".
 01  WK-MSG1           PIC  N(08)
                       VALUE NC"発注データ有り。".
 01  WK-MSG2           PIC  N(08)
                       VALUE NC"発注データ無し。".
 01  WK-MSG3           PIC  N(15)
                       VALUE NC"振分できない事業所データ有り。".
 01  WK-MSG4           PIC  N(15)
                       VALUE NC"振分できない事業所データ無し。".
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-YYMD           PIC  9(08).
 01  FILLER             REDEFINES      SYS-YYMD.
     03  SYS-YYYY       PIC  9(04).
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
****************************************************************
*    プリントエリア                                            *
****************************************************************
*--------------------------------------------------------------*
*    ヘッダ                                                    *
*--------------------------------------------------------------*
*
 01  HD1.
     03  FILLER                  PIC  X(05)  VALUE  SPACE.
     03  HD1-00                  PIC  X(08).
     03  FILLER                  PIC  X(10)  VALUE  SPACE.
     03  HD1-005                 PIC  N(02)
                                 CHARACTER  TYPE  IS  CHR-21.
     03  FILLER                  PIC  X(07)  VALUE  SPACE.
     03  FILLER                  PIC  N(23)  VALUE
         NC"※※　　　リック事業部別件数リスト　　　　※※"
                                 CHARACTER  TYPE  IS  CHR-21.
     03  FILLER                  PIC  X(08)  VALUE  SPACE.
     03  HD1-01                  PIC  9(04).
     03  FILLER                  PIC  N(01)  VALUE  NC"年"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  HD1-02                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"月"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  HD1-03                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"日"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(03)  VALUE  SPACE.
     03  HD1-04                  PIC  ZZ9.
     03  FILLER                  PIC  N(01)  VALUE  NC"頁"
                                 CHARACTER  TYPE  IS  CHR-2.
*
 01  HD2.
     03  FILLER                  PIC  X(35)  VALUE  SPACE.
     03  FILLER                  PIC  N(03)  VALUE
                                 NC"事業部"
                                 CHARACTER   TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(22)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"発注件数"
                                 CHARACTER   TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(05)  VALUE  SPACE.
*
 01  SEN                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(18)  VALUE
         NC"──────────────────".
 01  SEN1.
     03  FILLER                  PIC  X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                  PIC  X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                  PIC  X(36)  VALUE
         "------------------------------------".
 01  DT1                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(35)  VALUE  SPACE.
     03  DT1-01                  PIC  9(04).
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  DT1-02                  PIC  N(08).
     03  FILLER                  PIC  X(04)  VALUE  SPACE.
     03  DT1-03                  PIC  Z,ZZZ,ZZ9.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  DT1-04                  PIC  N(15).
 01  DT2                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(35)  VALUE  SPACE.
     03  DT2-01                  PIC  N(14).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
 LINKAGE                SECTION.
 01  PARA-CHK1               PIC X(01).
 01  PARA-CHK2               PIC X(01).
****************************************************************
 PROCEDURE              DIVISION  USING  PARA-CHK1
                                         PARA-CHK2.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
 LICFILE1ERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      LICFILE1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH8910L LICFILE1 ERROR " LC1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 LICFILE2ERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      LICFILE2.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH8910L LICFILE2 ERROR " LC2-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 LICFILE3ERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      LICFILE3.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH8910L LICFILE3 ERROR " LC3-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN.
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SJH8910L START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     LICFILE1 LICFILE2 LICFILE3.
     OPEN     OUTPUT    PRTFILE.
*クリア
     INITIALIZE    WK-CNT  FLAGS.
*ヘッダ行印字
     PERFORM       HEAD-WT-SEC.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*リック（資材）
*    件数カウント
     PERFORM LICFILE1-RD-SEC  UNTIL  LC1-FLG = "END".
*    帳票エリアセット
     IF   LC1-CNT  >  ZERO
          MOVE    02             TO  DT1-01
          MOVE NC"リック（資材）　"  TO  DT1-02
          MOVE    LC1-CNT        TO  DT1-03
          MOVE    WK-MSG1        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
          MOVE    "1"            TO  PARA-CHK1
     ELSE
          MOVE    02             TO  DT1-01
          MOVE NC"リック（資材）　"  TO  DT1-02
          MOVE    ZERO           TO  DT1-03
          MOVE    WK-MSG2        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
          MOVE    "2"            TO  PARA-CHK1
     END-IF.
*リック（植物）
*    件数カウント
     PERFORM LICFILE2-RD-SEC  UNTIL  LC2-FLG = "END".
*    帳票エリアセット
     IF   LC2-CNT > ZERO
          MOVE    03             TO  DT1-01
          MOVE NC"リック（植物）　"  TO  DT1-02
          MOVE    LC2-CNT        TO  DT1-03
          MOVE    WK-MSG1        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
          MOVE    "1"            TO  PARA-CHK2
     ELSE
          MOVE    03             TO  DT1-01
          MOVE NC"リック（植物）　"  TO  DT1-02
          MOVE    ZERO           TO  DT1-03
          MOVE    WK-MSG2        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
          MOVE    "2"            TO  PARA-CHK2
     END-IF.
*リック（以外）
*    件数カウント
     PERFORM LICFILE3-RD-SEC  UNTIL  LC3-FLG = "END".
*    帳票エリアセット
     IF   LC3-CNT > ZERO
          MOVE    04             TO  DT1-01
          MOVE NC"リック（以外）　"  TO  DT1-02
          MOVE    LC3-CNT        TO  DT1-03
          MOVE    WK-MSG1        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     ELSE
          MOVE    04             TO  DT1-01
          MOVE NC"リック（以外）　"  TO  DT1-02
          MOVE    LC3-CNT        TO  DT1-03
          MOVE    WK-MSG2        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*    帳票エリアセット
     IF   LC3-CNT > ZERO
          MOVE    99             TO  DT1-01
          MOVE NC"振分エラー　　　"  TO  DT1-02
          MOVE    LC3-CNT        TO  DT1-03
          MOVE    WK-MSG3        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     ELSE
          MOVE    99             TO  DT1-01
          MOVE NC"振分エラー　　　"  TO  DT1-02
          MOVE    LC3-CNT        TO  DT1-03
          MOVE    WK-MSG4        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*    件数チェック
     IF   CHK-FLG = SPACE
          MOVE    WK-MSG2        TO  DT2-01
          WRITE   PRT-REC   FROM   DT2    AFTER  5
     END-IF.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    LICFILE1 LICFILE2 LICFILE3.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SJH8910L END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    リック（資材）
*--------------------------------------------------------------*
 LICFILE1-RD-SEC             SECTION.
     READ   LICFILE1   AT  END
            MOVE  "END"  TO  LC1-FLG
            NOT   AT  END
            ADD    1     TO  LC1-CNT
     END-READ.
 LICFILE1-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    リック（植物）
*--------------------------------------------------------------*
 LICFILE2-RD-SEC             SECTION.
     READ   LICFILE2   AT  END
            MOVE  "END"  TO  LC2-FLG
            NOT   AT  END
            ADD    1     TO  LC2-CNT
     END-READ.
 LICFILE2-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    リック（以外）
*--------------------------------------------------------------*
 LICFILE3-RD-SEC             SECTION.
     READ   LICFILE3   AT  END
            MOVE  "END"  TO  LC3-FLG
            NOT   AT  END
            ADD    1     TO  LC3-CNT
     END-READ.
 LICFILE3-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*             ヘッダ部出力処理                                 *
*--------------------------------------------------------------*
 HEAD-WT-SEC                  SECTION.
*項目設定
***  プログラムＩＤ
     MOVE     PG-ID               TO        HD1-00.
***  日付
     MOVE     "3"                 TO        LINK-IN-KBN.
     MOVE     SYS-DATE            TO        LINK-IN-YMD6.
     MOVE     ZERO                TO        LINK-IN-YMD8.
     MOVE     ZERO                TO        LINK-OUT-RET.
     MOVE     ZERO                TO        LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING        LINK-IN-KBN
                                            LINK-IN-YMD6
                                            LINK-IN-YMD8
                                            LINK-OUT-RET
                                            LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD(1:4)   TO        HD1-01.
     MOVE     LINK-OUT-YMD(5:2)   TO        HD1-02.
     MOVE     LINK-OUT-YMD(7:2)   TO        HD1-03.
***  ページ■
     MOVE     1                   TO        HD1-04.
*
     MOVE     SPACE               TO        HD1-005.
*
*ヘッダ部出力
     WRITE    PRT-REC      FROM   HD1       AFTER  3.
     WRITE    PRT-REC      FROM   SEN       AFTER  2.
     WRITE    PRT-REC      FROM   HD2       AFTER  1.
     WRITE    PRT-REC      FROM   SEN       AFTER  1.
 HEAD-WT-EXIT.
     EXIT.
